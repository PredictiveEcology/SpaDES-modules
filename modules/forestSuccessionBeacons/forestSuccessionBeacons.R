###
### MODULE: forestSuccessionBeacons
###
### DESCRIPTION: a basic forest succession module
###               - land cover classes (2005) for Canada
###               - etc.
###

### load any required packages
### (use `loadPackages` or similar)
pkgs <- list("SpaDES", "raster", "RColorBrewer")
loadPackages(pkgs)
rm(pkgs)

### event functions
doEvent.forestSuccessionBeacons <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {

    ### check for module dependencies:
    ### (use NULL if no dependencies exist)
    depends <- NULL

    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # if a required module isn't loaded yet,
    # reschedule this module init for later
    if (reloadModuleLater(sim, depends)) {
      sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSuccession", "init")
    } else {
        # do stuff for this event
        sim <- forestSuccessionInit(sim)

        # schedule the next event
#        sim <- scheduleEvent(sim, 0.5, "forestSuccessionBeacons", "succession")
        sim <- scheduleEvent(sim, simParams(sim)$forestSuccessionBeacons$startTime,
                             "forestSuccessionBeacons", "succession")
        sim <- scheduleEvent(sim, simParams(sim)$forestSuccessionBeacons$.saveInterval,
                             "forestSuccessionBeacons", "save")
        sim <- scheduleEvent(sim, simParams(sim)$forestSuccessionBeacons$.plotInitialTime,
                             "forestSuccessionBeacons", "plot.init")

    }
  } else if (eventType=="succession") {
    # do stuff for this event
    sim <- forestSuccessionSuccession(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) +
                           simParams(sim)$forestSuccessionBeacons$returnInterval,
                         "forestSuccessionBeacons", "succession")
#    sim <- scheduleEvent(sim, simCurrentTime(sim)+1.0, "forestSuccessionBeacons", "succession")
  } else if (eventType=="plot.init") {
    # do stuff for this event
    #setColors(vegMap) <- vegMapColors[minValue(vegMap):maxValue(vegMap)]
    Plot(vegMap)
    Plot(trajMap)
#     grid.rect(just="topright", y=1.05, width=0.3, height=0.1, gp=gpar(fill="white", col="white"))
#     grid.text(paste("vegMap: Time",simCurrentTime(sim)),y=1.05)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+ simParams(sim)$forestSuccessionBeacons$.plotInterval,
                         "forestSuccessionBeacons", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(vegMap)

    # ggplot
    labelsShort <- sapply(strsplit(as.character(levels(vegMap)[[1]]$Class)," "),
                          function(x) paste(substr(x, 1, 3),collapse="_"))
    veg <- data.frame(veg=sort(na.omit(getValues(vegMap))))
    histColors <- getColors(vegMap)$layer
    vegTypeDistribution <- ggplot(veg, aes(factor(veg), fill=factor(veg)), xlab="Vegetation Type") +
      geom_bar() +
      scale_fill_manual(values=histColors[as.numeric(as.character(unique(veg$veg)))]) +
      scale_x_discrete(breaks = 1:11, labels=labelsShort) +
      theme(axis.text.x = element_text(size=6, angle = 45, hjust = 1, colour="black"),
            axis.text.y = element_text(size=6, colour="black"),
            axis.title.x = element_text(size=8, colour="black"),
            axis.title.y = element_text(size=8, colour="black"),
            legend.position="none")
    assign("vegTypeDistribution", vegTypeDistribution, envir=.GlobalEnv)
    Plot(vegTypeDistribution)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim)+ simParams(sim)$forestSuccessionBeacons$.plotInterval,
                         "forestSuccessionBeacons", "plot")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1,"eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE], "\'", sep=""))
  }
  return(invisible(sim))
}

forestSuccessionInit <- function(sim) {

  vegMap <<- vegMapBeacons
  trajMap <<- trajMapBeacons

  simModulesLoaded(sim) <- append(simModulesLoaded(sim), "forestSuccessionBeacons")

  return(invisible(sim))
}

forestSuccessionSuccession <- function(sim) {

  ageMap.v <- round(getValues(ageMap))+1
  trajMap.v <- getValues(trajMap)

  vegMap <<- setValues(vegMap,trajObj[cbind(ageMap.v,trajMap.v)])

  return(invisible(sim))
}

