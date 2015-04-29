###
### name:         caribouMovementLcc
###
### description:  Simulate caribou movement via correlated random walk.
###
### keywords:     caribou; individual based movement model; correlated random walk
###
### authors:      Eliot J. B. McIntire <Eliot.McIntire@NRCan.gc.ca>
###
### version:      0.2.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     31557600 (1 year)
###
### citation:     NA
###
### reqdPkgs:     grid; raster; sp
###
### parameters:   paramName: glmInitialTime
###               paramClass: numeric
###               default: 1.00
###
###               paramName: moveInterval
###               paramClass: numeric
###               default: 1.0
###
###               paramName: startTime
###               paramClass: numeric
###               default: 1.0
###
###               paramName: .plotInitialTime
###               paramClass: numeric
###               default: 0
###
###               paramName: .plotInterval
###               paramClass: numeric
###               default: 1
###
###               paramName: .saveInitialTime
###               paramClass: numeric
###               default: NA
###
###               paramName: .saveInterval
###               paramClass: numeric
###               default: NA
###
### inputObjects: objectName: vegMap
###               objectClass: RasterLayer
###               other: NA
###
### outputObjects: objectName: caribou
###                objectClass: SpatialPointsDataFrame
###                other: NA
###
###                objectName: caribouRas
###                objectClass: RasterLayer
###                other: NA
###
###                objectName: glmPlot
###                objectClass: gg
###                other: NA
###
###                objectName: glmPVals
###                objectClass: numeric
###                other: NA
###
### caribouMovementLcc module metadata
defineModule(sim, list(
  name="caribouMovementLcc",
  description="Simulate caribou movement via correlated random walk.",
  keywords=c("caribou", "individual based movement model", "correlated random walk"),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("0.2.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=31557600,
  citation=list(),
  reqdPkgs=list("grid", "raster", "sp"),
  parameters=rbind(
    defineParameter("glmInitialTime", "numeric", 100.0),
    defineParameter("glmInterval", "numeric", 10.0),
    defineParameter("N", "numeric", 100.0),
    defineParameter("moveInterval", "numeric", 1.0),
    defineParameter("startTime", "numeric", 1.0),
    defineParameter(".plotInitialTime", "numeric", 0),
    defineParameter(".plotInterval", "numeric", 1),
    defineParameter(".saveInitialTime", "numeric", NA_real_),
    defineParameter(".saveInterval", "numeric", NA_real_)),
  inputObjects=data.frame(objectName=c("ageMap", "vegMap"),
                          objectClass=c("RasterLayer", "RasterLayer"),
                          other=rep(NA_character_, 2L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("caribou", "caribouRas", "glmPlot", "glmPVals"),
                           objectClass=c("SpatialPointsDataFrame", "RasterLayer",
                                         "gg", "numeric"),
                           other=rep(NA_character_, 4L), stringsAsFactors=FALSE)
))

### event functions
doEvent.caribouMovementLcc <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject("vegMap")
    # do stuff for this event
    sim <- caribouMovementInit(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simParams(sim)$caribouMovementLcc$startTime, "caribouMovementLcc", "move")
    sim <- scheduleEvent(sim, simParams(sim)$caribouMovementLcc$.plotInitialTime, "caribouMovementLcc", "plot.init")
    sim <- scheduleEvent(sim, simParams(sim)$caribouMovementLcc$glmInitialTime, "caribouMovementLcc", "glm.init")

  } else if (eventType=="move") {
    # do stuff for this event
    sim <- caribouMovementMove(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$moveInterval, "caribouMovementLcc", "move")
  } else if (eventType=="plot.init") {
    # do stuff for this event

    #This wipes out previous values of the caribou map with a white raster
#     #a = try(seekViewport("caribou"), silent = TRUE)
#     #if(!is(a, "try-error")) {
#       try(grid.rect(unit(1, "npc"), unit(1, "npc"), draw=TRUE, vp="caribouRas",
#                 width = unit(1, units="npc"), height=unit(1, units="npc"),
#                 gp=gpar(fill="white", col="white"), just=c(1,1)), silent=TRUE)
#     #}

    #dependencies <<- depsGraph(sim, plot=TRUE)
    #Plot(dependencies, vertex.size=60, vertex.label.cex=0.65)

    Plot(caribouRas, pch=19, size=0.1, zero.color = "white", legendRange=c(0, simStopTime(sim)))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$.plotInterval, "caribouMovementLcc", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(caribouRas, pch=19, size=0.1, zero.color = "white", legendRange=c(0, simStopTime(sim)))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$.plotInterval, "caribouMovementLcc", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$.saveInterval, "caribouMovementLcc", "save")

  } else if (eventType=="glm.init") {

    # do stuff for this event
    glmPVals <- numeric(simStopTime(sim))
    glmCaribouFire <- glm(getValues(caribouRas)~getValues(FiresCumul))
    glmPVals[1] <- summary(glmCaribouFire)[[13]]["getValues(FiresCumul)",'Pr(>|t|)']
    glmPlot <- qplot((simParams(sim)$caribouMovementLcc$glmInitialTime):simCurrentTime(sim),
          glmPVals[simParams(sim)$caribouMovementLcc$glmInitialTime:simCurrentTime(sim)],
          xlab="Simulation Time", ylab="p value: caribou ~ Fire",
          xlim=c(simParams(sim)$caribouMovementLcc$glmInitialTime,simStopTime(sim)))
    glmPlot <- glmPlot +
      theme(axis.text.x = element_text(size=10, colour="black"),
          axis.text.y = element_text(size=10, colour="black"),
          axis.title.x = element_text(size=12, colour="black"),
          axis.title.y = element_text(size=12, colour="black"),
          legend.position="none")

    assign("glmPlot", glmPlot, envir=.GlobalEnv)
    assign("glmPVals", glmPVals, envir=.GlobalEnv)
    Plot(glmPlot)
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$glmInterval, "caribouMovementLcc", "glm.plot")
    # schedule the next event

  } else if (eventType=="glm.plot") {
    # do stuff for this event
    glmCaribouFire <- glm(getValues(caribouRas)~getValues(FiresCumul))
    glmPVals[round(simCurrentTime(sim))] <- summary(glmCaribouFire)[[13]]["getValues(FiresCumul)",'Pr(>|t|)']
    glmPlot <- qplot((simParams(sim)$caribouMovementLcc$glmInitialTime):simCurrentTime(sim),
                     glmPVals[simParams(sim)$caribouMovementLcc$glmInitialTime:simCurrentTime(sim)],
                     xlab="Simulation Time", ylab="p value: caribou ~ Fire",
                     xlim=c(simParams(sim)$caribouMovementLcc$glmInitialTime,simStopTime(sim)))
    glmPlot <- glmPlot +
      theme(axis.text.x = element_text(size=10, colour="black"),
            axis.text.y = element_text(size=10, colour="black"),
            axis.title.x = element_text(size=12, colour="black"),
            axis.title.y = element_text(size=12, colour="black"),
            legend.position="none")
    assign("glmPlot", glmPlot, envir=.GlobalEnv)
    assign("glmPVals", glmPVals, envir=.GlobalEnv)
    Plot(glmPlot)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$caribouMovementLcc$glmInterval, "caribouMovementLcc", "glm.plot")

  } else {
    warning(paste("Undefined event type: \'",simEvents(sim)[1,"eventType",with=FALSE],
                  "\' in module \'", simEvents(sim)[1,"moduleName",with=FALSE],"\'",sep=""))
  }
  return(invisible(sim))
}

caribouMovementInit <- function(sim) {
  cellsFromXY <<- cellFromXY # the raster Package has a bug
  #vegMap <- getGlobal("vegMap")
  caribouRas <- raster(extent(getGlobal("vegMap")), ncol=ncol(getGlobal("vegMap")), nrow=nrow(getGlobal("vegMap")), vals=0)
  setColors(caribouRas, n=simStopTime(sim)) <-
    c("white", colorRampPalette(colors = c("grey","black"))(ceiling(simStopTime(sim)-1)))


  yrange <- c(ymin(getGlobal("vegMap")), ymax(getGlobal("vegMap")))
  xrange <- c(xmin(getGlobal("vegMap")), xmax(getGlobal("vegMap")))
#    best <- max(values(getGlobal("vegMap")))
#    worst <- min(values(getGlobal("vegMap")))
#    good <- Which(getGlobal("vegMap")>0.8*best)
#
#   al <- agentLocation(good)    # good getGlobal("vegMap"), from above
#   initialCoords <- probInit(getGlobal("vegMap"), al)

  # initialize caribou agents
  N <- simParams(sim)$caribouMovementLcc$N
  IDs <- as.character(1:N)
  sex <- sample(c("female", "male"), N, replace=TRUE)
  age <- round(rnorm(N, mean=8, sd=3))
  x1 <- rep(0, N)
  y1 <- rep(0, N)
  starts <- cbind(x=runif(N, xrange[1],xrange[2]),
                  y=runif(N, yrange[1],yrange[2]))

#browser()
  # create the caribou agent object
  caribou <- SpatialPointsDataFrame(coords=starts,
                                     data=data.frame(x1, y1, sex, age))
  
  row.names(caribou) <- IDs # alternatively, add IDs as column in data.frame above
  caribouRas[caribou] <- caribouRas[caribou]+1
  caribouRas[is.na(getGlobal("vegMap"))] <- NA
  assignGlobal("caribouRas", caribouRas)
  assignGlobal("caribou", caribou)

  return(invisible(sim))
}

caribouMovementMove <- function(sim) {
  # crop any caribou that went off maps

  caribou <- getGlobal("caribou")
  if(length(caribou)==0) stop("All agents are off map")

  # find out what pixels the individuals are on now
  ex <- getGlobal("ageMap")[caribou]

  # step length is a function of current cell's habitat quality
  sl <- 20/log(ex+5)

  ln <- rlnorm(length(ex), sl, 0.02) # log normal step length
  sd <- 30 # could be specified globally in params

  caribou <- move("crw", caribou, stepLength=ln, stddev=sd, lonlat=FALSE)
  
  caribou <- crop(caribou, getGlobal("vegMap"))

  caribouRas[caribou] <- caribouRas[caribou] + 1
#   setColors(caribouRas, n=simStopTime(sim)) <-
#     c("white", colorRampPalette(colors = c("grey","black"))(simStopTime(sim)/2-1))

  assignGlobal("caribou", caribou)
  assignGlobal("caribouRas", caribouRas)

#     #rads <- sample(10:30, length(caribou), replace=TRUE)
#     #rings <- cir(caribou, radiuses=rads, vegMap, 1)
#     #points(rings$x, rings$y, col=rings$ids, pch=19, cex=0.1)

    return(invisible(sim))
}
