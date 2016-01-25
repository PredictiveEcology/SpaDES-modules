stopifnot(packageVersion("SpaDES") >= "1.1.0")

defineModule(sim, list(
  name = "caribouMovementLcc",
  description = "Simulate caribou movement via correlated random walk.",
  keywords = c("caribou", "individual based movement model", "correlated random walk"),
  childModules = character(),
  authors = c(person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre"))),
  version = numeric_version("1.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "month",
  citation = list("citation.bib"),
  documentation = list("README.txt", "caribouMovementLcc.Rmd"),
  reqdPkgs = list("grid", "raster", "sp"),
  parameters = rbind(
    defineParameter("glmInitialTime", "numeric", 100.0, NA, NA, desc = "Simulation time for doing first glm fit
                    between cumulative caribou and cumulative caribou"),
    defineParameter("glmInterval", "numeric", 10.0, NA, NA, desc = "Time interval between glm fitting"),
    defineParameter("N", "numeric", 100.0, 0.0, 10000.0, desc = "Number of caribou to intiate at startTime"),
    defineParameter("torus", "logical", TRUE, NA, NA, desc = "Whether caribou should wrap around map if they go off map sides"),
    defineParameter("moveInterval", "numeric", 1.0, NA, NA, desc = "Time interval between move events"),
    defineParameter("startTime", "numeric", 1.0, NA, NA, desc = "Simulation time at which to initiate caribou movement"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, desc = "Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc = "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc = "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc = "Interval between save events")),
  inputObjects = data.frame(
    objectName = c("ageMap", "vegMap"),
    objectClass = c("RasterLayer", "RasterLayer"),
    sourceURL  =  c(NA_character_, NA_character_),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("caribou", "caribouRas", "glmPlot", "glmPVals"),
    objectClass = c("SpatialPointsDataFrame", "RasterLayer", "gg", "numeric"),
    other = rep(NA_character_, 4L), stringsAsFactors = FALSE)
))

### event functions
doEvent.caribouMovementLcc <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(sim, "vegMap")
    # do stuff for this event
    sim <- sim$caribouMovementInit(sim)


    # schedule the next event
    sim <- scheduleEvent(sim, params(sim)$caribouMovementLcc$startTime, "caribouMovementLcc", "move")
    sim <- scheduleEvent(sim, params(sim)$caribouMovementLcc$.plotInitialTime, "caribouMovementLcc", "plot.init")
    sim <- scheduleEvent(sim, params(sim)$caribouMovementLcc$glmInitialTime, "caribouMovementLcc", "glm.init")

  } else if (eventType == "move") {
    # do stuff for this event
    sim <- sim$caribouMovementMove(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$caribouMovementLcc$moveInterval,
                         "caribouMovementLcc", "move")
  } else if (eventType == "plot.init") {
    # do stuff for this event

    # This wipes out previous values of the caribou map with a white raster
    Plot(sim$caribouRas, zero.color="white",
         legendRange=c(0, end(sim, "year")-start(sim, "year")))

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$caribouMovementLcc$.plotInterval,
                         "caribouMovementLcc", "plot")
  } else if (eventType == "plot") {
    # do stuff for this event
    Plot(sim$caribouRas, zero.color="white",
         legendRange=c(0, end(sim, "year")-start(sim, "year")))

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$caribouMovementLcc$.plotInterval,
                         "caribouMovementLcc", "plot")
  } else if (eventType == "save") {
    # do stuff for this event
    sim <- saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$caribouMovementLcc$.saveInterval,
                         "caribouMovementLcc", "save")

  } else if (eventType == "glm.init") {
    # do stuff for this event
    sim$glmPVals <- rep(NA_real_,end(sim, "month"))
    glmCaribouFire <- glm(getValues(sim$caribouRas)~getValues(sim$FiresCumul))
    sim$glmPVals[time(sim)] <- summary(glmCaribouFire)[[13]]["getValues(sim$FiresCumul)",'Pr(>|t|)']
    glmPlot <- qplot(((params(sim)$caribouMovementLcc$glmInitialTime):
                        time(sim,"month")-start(sim))/12+
                       start(sim),
                     sim$glmPVals[params(sim)$caribouMovementLcc$glmInitialTime:time(sim,"month")],
                     xlab="Simulation Time", ylab="p value: caribou ~ Fire",
                     xlim=c((params(sim)$caribouMovementLcc$glmInitialTime-start(sim))/12+start(sim),
                            end(sim,"year")))
    glmPlot <- glmPlot +
      theme(axis.text.x = element_text(size=10, colour="black"),
          axis.text.y = element_text(size=10, colour="black"),
          axis.title.x = element_text(size=12, colour="black"),
          axis.title.y = element_text(size=12, colour="black"),
          legend.position="none")

    Plot(glmPlot)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$caribouMovementLcc$glmInterval,
                         "caribouMovementLcc", "glm.plot")
    # schedule the next event

  } else if (eventType == "glm.plot") {
    # do stuff for this event
    glmCaribouFire <- glm(getValues(sim$caribouRas)~getValues(sim$FiresCumul))
    sim$glmPVals[round(time(sim, "month"))] <-
      summary(glmCaribouFire)[[13]]["getValues(sim$FiresCumul)",'Pr(>|t|)']
    glmPlot <- qplot(((params(sim)$caribouMovementLcc$glmInitialTime):
                        time(sim,"month")-start(sim))/12+
                       start(sim),
                     sim$glmPVals[params(sim)$caribouMovementLcc$glmInitialTime:time(sim,"month")],
                     xlab="Simulation Time", ylab="p value: caribou ~ Fire",
                     xlim=c((params(sim)$caribouMovementLcc$glmInitialTime-start(sim))/12+start(sim),
                            end(sim,"year")))
    glmPlot <- glmPlot +
      theme(axis.text.x = element_text(size=10, colour="black"),
            axis.text.y = element_text(size=10, colour="black"),
            axis.title.x = element_text(size=12, colour="black"),
            axis.title.y = element_text(size=12, colour="black"),
            legend.position="none")
    Plot(glmPlot)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$caribouMovementLcc$glmInterval,
                         "caribouMovementLcc", "glm.plot")

  } else {
    warning(paste("Undefined event type: \'",events(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", events(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
  }
  return(invisible(sim))
}

caribouMovementInit <- function(sim) {
  cellsFromXY <<- cellFromXY # the raster Package has a bug
  #vegMap <- sim$vegMap
  sim$caribouRas <- raster(extent(sim$vegMap), ncol=ncol(sim$vegMap), nrow=nrow(sim$vegMap), vals=0)
  #sim$caribouRas <- as(sim$caribouRas, "RasterLayerSparse")
  setColors(sim$caribouRas, n=end(sim, unit="year")-start(sim, unit="year")) <-
     c("light grey","black")

  yrange <- c(ymin(sim$vegMap), ymax(sim$vegMap))
  xrange <- c(xmin(sim$vegMap), xmax(sim$vegMap))

  # initialize caribou agents
  N <- params(sim)$caribouMovementLcc$N
  IDs <- as.character(1:N)
  sex <- sample(c("female", "male"), N, replace=TRUE)
  age <- round(rnorm(N, mean=8, sd=3))
  x1 <- rep(0, N)
  y1 <- rep(0, N)
  starts <- cbind(x=runif(N, xrange[1],xrange[2]),
                  y=runif(N, yrange[1],yrange[2]))

  # create the caribou agent object
  sim$caribou <- SpatialPointsDataFrame(coords=starts,
                                     data=data.frame(x1, y1, sex, age))

  row.names(sim$caribou) <- IDs # alternatively, add IDs as column in data.frame above
  sim$caribouRas[sim$caribou] <- sim$caribouRas[sim$caribou]+1
  sim$caribouRas[is.na(sim$vegMap)] <- NA

  return(invisible(sim))
}

caribouMovementMove <- function(sim) {
  # crop any caribou that went off maps

  if(length(sim$caribou) == 0) stop("All agents are off map")

  # find out what pixels the individuals are on now
  ex <- sim$ageMap[sim$caribou]
  # assume age = 0 if no age data.
  ex[is.na(ex)] <- 0

  # step length is a function of current cell's habitat quality
  sl <- 20/log(ex+10)

  # log normal step length, will return NA if ex is off map
  ln <- suppressWarnings(rlnorm(length(ex), sl, 0.02))
  sd <- 30 # could be specified globally in params

  sim$caribou <- move("crw", sim$caribou, stepLength=ln, stddev=sd,
                      lonlat=FALSE, extent=extent(sim$vegMap),
                      torus=params(sim)$caribouMovementLcc$torus)

  sim$caribou <- crop(sim$caribou, sim$vegMap)

  sim$caribouRas[sim$caribou] <- sim$caribouRas[sim$caribou] + 1

  return(invisible(sim))
}
