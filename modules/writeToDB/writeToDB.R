stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="writeToDB",
  description="Writes updates to the database, including progress and status, as well as saving snapshots.",
  keywords=c("database"),
  authors=c(person(c("Gregory", "S"), "Zhang", email="Gregory.Zhang@NRCan-RNCan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("0.1.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timeunit=NA,
  citation=list(),
  reqdPkgs=list("RPostgreSQL", "magrittr", "dplyr", "igraph"),
  parameters=rbind(
    defineParameter("dbname", "character", "", NA, NA),
    defineParameter("host", "character", "", NA, NA),
    defineParameter("port", "numeric", 0L, NA, NA),
    defineParameter("user", "character", "", NA, NA),
    defineParameter("password", "character", "", NA, NA),
    defineParameter("updateInterval", "numeric", 1, NA, NA),
    defineParameter("outputTable", "character", "", NA, NA),
    defineParameter("updateID", "numeric", 0L, NA, NA),
    defineParameter("repID", "numeric", 1L, NA, NA),
    defineParameter("startTime", "numeric", 0, NA, NA),
    defineParameter("timestep", "numeric", 1, NA, NA),
    defineParameter("fileOut", "character", "", NA, NA)),
  inputObjects=data.frame(objectName=NA_character_,
                          objectClass=NA_character_,
                          other=NA_character_, stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=NA_character_,
                           objectClass=NA_character_,
                           other=NA_character_, stringsAsFactors=FALSE)
))

### event functions
doEvent.writeToDB <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    sim <- scheduleEvent(sim, times(sim)$start + params(sim)$writeToDB$updateInterval, "writeToDB", "update")
    sim <- scheduleEvent(sim, times(sim)$stop, "writeToDB", "update")
    sim <- scheduleEvent(sim, times(sim)$stop, "writeToDB", "saveFiles")

    params <- params(sim)$writeToDB
    con <- dbConnect(PostgreSQL(), dbname=params$dbname, user=params$user, password=params$password,
                     host=params$host, port=params$port)

    assign(globals(sim)$writeToDBCon, con, envir=.GlobalEnv)
    assign(globals(sim)$writeToDBIndex, 1, envir=.GlobalEnv)
    assign(globals(sim)$writeToDBFileList, list.files(getwd(), recursive=TRUE), envir=.GlobalEnv) #saves list of files in all subdirectories

    dir.create(outputFilePath(sim)) #create output directory

    dev.new(file=NULL)
    dev.control(displaylist="enable") #allows copying

    #draw dependency graph
    if(params(sim)$writeToDB$repID == 1){
      depsGraphFilename <- paste0(outputFilePath(sim), params(sim)$writeToDB$updateID, "-depsgraph.png")
      dev.new(file=NULL)
      dev.control(displaylist="enable")
      plot(depsGraph(sim, TRUE))
      dev.copy(png, filename=depsGraphFilename, width=750, height=750)
      dbGetQuery(con, sqlInsert("outputdata", simid=params(sim)$writeToDB$updateID, filepath=depsGraphFilename, type="depsgraph"))
      dev.off()
    }

    inputs(sim) <- append(inputs(sim), "writeToDB")
  } else if (eventType=="update") {
    events <- events(sim)
    currTimeEvents <- events[!(events$moduleName %in% c("writeToDB", "progress"))
                             & events$eventTime %==% time(sim)]
    if(nrow(currTimeEvents) > 0){
      sim <- scheduleEvent(sim, time(sim), "writeToDB", "update") #schedule after any events at the same time
      return(invisible(sim))
    }

    writeToDBEvents <- events[events$moduleName == "writeToDB" &
                                lapply(events$eventTime, function(x){x %==% time(sim)}) %>% unlist]

    if(nrow(writeToDBEvents) <= 1){ #prevents this from running multiple times at same event time
      con <- get(globals(sim)$writeToDBCon, envir=.GlobalEnv)
      writeToDBUpdate(sim, con) #update database with current time

      if(params(sim)$writeToDB$repID == 1 && dev.cur() != 1){ #check devices to make sure not writing to null device
        if(params(sim)$writeToDB$timestep < 86400){ #not a date
          filenameDatePiece <- floor(time(sim) * params(sim)$writeToDB$timestep)
        }
        else {
          startDate <- params(sim)$writeToDB$startTime
          simTimestepDays <- params(sim)$writeToDB$timestep / 86400
          currDate <- startDate + (time(sim) * simTimestepDays)
          filenameDatePiece <- format(currDate, "%d%m%Y")
        }

        filename <- paste0(outputFilePath(sim), params(sim)$writeToDB$updateID, "-", filenameDatePiece, ".png")
        dev.copy(png, filename=filename, width=750, height=750) #create png from plot device
        dev.off()

        currIndex <- get(globals(sim)$writeToDBIndex, envir=.GlobalEnv)
        dbGetQuery(con, sqlInsert("outputsnapshots", simid=params(sim)$writeToDB$updateID,
                                  simtime=time(sim), filepath=filename, index=currIndex))
        assign(globals(sim)$writeToDBIndex, currIndex + 1, envir=.GlobalEnv)
      }

      if(time(sim) %==% times(sim)$stop){
        clearPlot()
      }
    }

    # schedule the next event
    plotEvents <- events[events$eventType == "plot"]
    nextPlot <- if(nrow(plotEvents) == 0) time(sim) else plotEvents[1,]$eventTime

    normEvents <- events[!(events$eventType %in% c("plot", "init", "save"))
                         & !(events$moduleName %in% c("writeToDB", "progress"))]
    nextEvent <- if(nrow(normEvents) == 0) time(sim) else normEvents[1,]$eventTime
    nextTime <- max(nextPlot, nextEvent, time(sim) + params(sim)$writeToDB$updateInterval) #try to schedule intelligently
    sim <- scheduleEvent(sim, nextTime, "writeToDB", "update")

  } else if(eventType=="saveFiles"){
    events <- events(sim)
    saveEvents <- events[!(events$moduleName %in% c("writeToDB", "progress"))
                             & events$eventTime %==% time(sim)
                            & events$eventType == "save"]
    if(nrow(saveEvents) > 0){
      sim <- scheduleEvent(sim, time(sim), "writeToDB", "update") #schedule after any saving events
      return(invisible(sim))
    }

    prevFiles <- get(globals(sim)$writeToDBFileList, envir=.GlobalEnv)
    currFiles <- list.files(getwd(), recursive=TRUE)
    newFiles <- setdiff(currFiles, prevFiles) #files created by simulation

    lapply(newFiles, function(filepath){
      newPath <- paste0(outputFilePath(sim), basename(filepath))
      con <- get(globals(sim)$writeToDBCon, envir=.GlobalEnv)
      file.rename(filepath, newPath) #move files to output folder
      dbGetQuery(con, sqlInsert("outputdata", simid=params(sim)$writeToDB$updateID, filepath=newPath, type="data"))
    })
  } else {
    warning(paste("Undefined event type: \'", events(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", events(sim)[1, "moduleName", with=FALSE] , "\'", sep=""))
  }
  return(invisible(sim))
}

writeToDBUpdate <- function(sim, con){
  outputTable <- params(sim)$writeToDB$outputTable
  updateID <- params(sim)$writeToDB$updateID
  paste("UPDATE", outputTable, "SET") %>%
    paste0(" currsimtime=", time(sim), ",") %>%
    paste0(" status='running'") %>%
    paste0(" WHERE id=", updateID) %>%
    dbGetQuery(con, .)

  if(time(sim) %==% times(sim)$stop){
    result <- paste("UPDATE", outputTable, "SET") %>%
      paste0(" completereps=completereps+1") %>%
      paste0(" WHERE id=", updateID) %>%
      paste0(" RETURNING completereps, replications") %>%
      dbGetQuery(con, .)
    if(result$completereps == result$replications){
      paste0("UPDATE ", outputTable, " SET status='complete' WHERE id=", updateID) %>%
        dbGetQuery(con, .)
    }
  }
}

sqlInsert <- function(tablename, toReturn=NULL, ...){
  argList <- list(...)
  values <- argList %>%
    lapply(escape) %>%
    paste(collapse=",")
  valueNames <- names(argList) %>%
    lapply(sql) %>%
    paste(collapse=",")
  if(!is.null(toReturn)) returnClause <- paste("RETURNING", toReturn)
  else returnClause <- NULL
  sql(paste0("INSERT INTO ", tablename, " (", valueNames, ") VALUES (", values, ") ", returnClause))
}

outputFilePath <- function(sim){
  #gives folder containing outputs for the simulation
  paste0(params(sim)$writeToDB$fileOut, params(sim)$writeToDB$updateID, "/")
}
