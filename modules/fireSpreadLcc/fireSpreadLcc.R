###
### name:         fireSpreadLcc
###
### description:  Simulate fire ignition and spread on a landscape, where
###               spread probability varies according to percent pine.
###               Fire size statistics are collected immediately after each burn event.
###               Requires a global simulation parameter `.stackName` be set.
###
### keywords:     fire; percolation model; spread algorithm
###
### authors:      Alex M. Chubaty <Alexander.Chubaty@NRCan.gc.ca>
###               Eliot J. B. McIntire <Eliot.McIntire@NRCan.gc.ca>
###               Steve Cumming <Steve.Cumming@sbf.ulaval.ca>
###
### version:      0.2.0
###
### spatialExtent: NA
###
### timeframe:    2005 - NA
###
### timestep:     31557600 (1 year)
###
### citation:     NA
###
### reqdPkgs:     ggplot2; methods; raster; RColorBrewer
###
### parameters:   paramName: drought
###               paramClass: numeric
###               default: 1.00
###
###               paramName: nfires
###               paramClass: numeric
###               default: 10L
###
###               paramName: its
###               paramClass: numeric
###               default: 1e6
###
###               paramName: persistprob
###               paramClass: numeric
###               default: 0.00
###
###               paramName: returnInterval
###               paramClass: logical
###               default: 10.0
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
###               objectName: simGlobals(sim)$burnStats
###               objectClass: numeric
###               other: NA
###
### outputObjects: objectName: Fires
###                objectClass: RasterLayer
###                other= NA
###
###                objectName: FiresCumul
###                objectClass: RasterLayer
###                other= NA
###
###                objectName: FireSizeDistribution
###                objectClass: gg
###                other= NA
###
###                objectName: simGlobals(sim)$burnStats
###                objectClass: numeric
###                other= NA
###
### fireSpreadLcc module metadata
defineModule(sim, list(
  name="fireSpreadLcc",
  description="Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `.stackName` be set.",
  keywords=c("fire", "percolation model", "spread algorithm"),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.2.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c("2005-01-01", NA)),
  timestep=31557600,
  citation=list(),
  reqdPkgs=list("ggplot2", "methods", "raster", "RColorBrewer"),
  parameters=rbind(
    defineParameter("drought", "numeric", 1.00),
    defineParameter("nFires", "numeric", 10L),
    defineParameter("its", "numeric", 1e6),
    defineParameter("persistprob", "numeric", 0.00),
    defineParameter("returnInterval", "numeric", 1.0),
    defineParameter("startTime", "numeric", 1.0),
    defineParameter(".plotInitialTime", "numeric", 1.0),
    defineParameter(".plotInterval", "numeric", 1),
    defineParameter(".saveInitialTime", "numeric", NA_real_),
    defineParameter(".saveInterval", "numeric", NA_real_)),
  inputObjects=data.frame(objectName=c("vegMap", simGlobals(sim)$burnStats),
                          objectClass=c("RasterLayer", "numeric"),
                          other=rep(NA_character_, 2L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("Fires",
                                        "FiresCumul",
                                        "FireSizeDistribution",
                                        simGlobals(sim)$burnStats),
                           objectClass=c("RasterLayer", "RasterLayer",
                                         "gg", "numeric"),
                           other=rep(NA_character_, 4L), stringsAsFactors=FALSE)
))

### event functions
doEvent.fireSpreadLcc <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(name="vegMap")

    # Clear global variable
    assignGlobal(simGlobals(sim)$burnStats, numeric())

    # do stuff for this event
    sim <- fireSpreadLccInit(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simParams(sim)$fireSpreadLcc$startTime, "fireSpreadLcc", "burn")
    sim <- scheduleEvent(sim, simParams(sim)$fireSpreadLcc$.saveInterval, "fireSpreadLcc", "save")
    sim <- scheduleEvent(sim, simParams(sim)$fireSpreadLcc$.plotInitialTime, "fireSpreadLcc", "plot.init")
  } else if (eventType=="burn") {
    # do stuff for this event
    sim <- fireSpreadLccBurn(sim)

    # schedule the next events
    sim <- scheduleEvent(sim, simCurrentTime(sim), "fireSpreadLcc", "stats") # do stats immediately following burn
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$returnInterval, "fireSpreadLcc", "burn")
  } else if (eventType=="stats") {
    # do stuff for this event
    sim <- fireSpreadLccStats(sim)

    # schedule the next event
    ## stats scheduling done by burn event
  } else if (eventType=="plot.init") {
    # do stuff for this event
    FiresCumul <- getGlobal("FiresCumul")
    Plot(FiresCumul,zero.color = "white")

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    FiresCumul <- getGlobal("FiresCumul")
    Plot(FiresCumul, zero.color="white")
    
    if(length(getGlobal(simGlobals(sim)$burnStats))>0) {
      
      nPixelsBurned <- getGlobal(simGlobals(sim)$burnStats)
  
      FireSizeDistribution <- qplot(main="", nPixelsBurned*6.25, xlab="Hectares")
      FireSizeDistribution <- FireSizeDistribution +
        theme(axis.text.x=element_text(size=10, angle = 45, hjust = 1, colour="black"),
              axis.text.y=element_text(size=10, colour="black"),
              axis.title.x=element_text(size=12, colour="black"),
              axis.title.y=element_text(size=12, colour="black"))
      assignGlobal("FireSizeDistribution", FireSizeDistribution) # for Plot, needs to be in Global Env
      suppressMessages(Plot(FireSizeDistribution))
    }

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) + simParams(sim)$fireSpreadLcc$.saveInterval, "fireSpreadLcc", "save")
  } else {
    warning(paste("Undefined event type: \'", simEvents(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", simEvents(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
  }
  return(invisible(sim))
}

fireSpreadLccInit <- function(sim) {
  ### create burn map that tracks fire locations over time
  Fires <- raster(extent(getGlobal("vegMap")), ncol=ncol(getGlobal("vegMap")),
                  nrow=nrow(getGlobal("vegMap")), vals=0)
  setColors(Fires,n=simParams(sim)$fireSpreadLcc$nFires+1) <-
    c("#FFFFFF", rev(heat.colors(simParams(sim)$fireSpreadLcc$nFires)))
  assignGlobal("Fires", Fires)
  #FiresCumul <<- Fires
  assignGlobal("FiresCumul", Fires)
  return(invisible(sim))
}

fireSpreadLccBurn <- function(sim) {
  #   assignGlobal("fireSpreadProb", reclassify(x=getGlobal("vegMap"),
  #                                             rcl=cbind(1:11,
  #                                                       c(0.225,0.225,0.21,0.15,0.15,0.18,0.1,0.1,0,0,0)*
  #                                                         simParams(sim)$fireSpreadLcc$drought)))
  fireSpreadProb <- reclassify(x=getGlobal("vegMap"),
                                  rcl=cbind(1:11,
                                            c(0.225,0.225,0.21,0.15,0.15,0.18,0.1,0.1,0,0,0)*
                                            simParams(sim)$fireSpreadLcc$drought))
  nFires <- rpois(1,simParams(sim)$fireSpreadLcc$nFires*simParams(sim)$fireSpreadLcc$drought)
  Fires <- spread(fireSpreadProb,
                   loci=as.integer(sample(1:ncell(fireSpreadProb), nFires)),
                   spreadProb=fireSpreadProb,
                   persistance=simParams(sim)$fireSpreadLcc$persistprob,
                   mask=NULL,
                   maxSize=1e8,
                   directions=8,
                   iterations=simParams(sim)$fireSpreadLcc$its,
                   plot.it=FALSE,
                   mapID=TRUE)
  Fires[is.na(getGlobal("vegMap"))] <- NA
  names(Fires) <- "Fires"
  setColors(Fires,n=nFires + 1) <- c("#FFFFFF", rev(heat.colors(nFires)))
  #  landscapes$Fires <- Fires

  FiresCumul <- getGlobal("FiresCumul")
  FiresCumul[] <- FiresCumul[] + (Fires[]>0)
  setColors(FiresCumul) <- c(colorRampPalette(c("orange", "darkred"))(maxValue(FiresCumul)+1))
  assignGlobal("Fires", Fires)
  assignGlobal("FiresCumul", FiresCumul)

  return(invisible(sim))
}

fireSpreadLccStats <- function(sim) {
  npix <- get(simGlobals(sim)$burnStats, envir=.GlobalEnv)

  assign(simGlobals(sim)$burnStats, c(npix, length(which(values(Fires)>0))), envir=.GlobalEnv)

  return(invisible(sim))
}

