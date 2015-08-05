stopifnot(packageVersion("SpaDES") >= "0.99.0")

defineModule(sim, list(
  name="fireSpreadLcc",
  description="Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `.stackName` be set.",
  keywords=c("fire", "percolation model", "spread algorithm"),
  childModules=character(),
  authors=c(person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.2.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c("2005-01-01", NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("ggplot2", "methods", "raster", "RColorBrewer"),
  parameters=rbind(
    defineParameter("drought", "numeric", 1.00, 0.8, 1.4, desc="An arbitrary index of drought, where 1 is 'normal', and greater than 1 is more dry"),
    defineParameter("nFires", "numeric", 10L, 0L, 100L, desc="Number of fires to initiate at each returnInterval"),
    defineParameter("its", "numeric", 1e6, NA, NA, desc="Maximum number of iterations for the spread algorithm"),
    defineParameter("persistprob", "numeric", 0.00, 0.00, 1.00, desc="Probability that a burning cell will continue burning for 1 iteration"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc="Time interval between fire events"),
    defineParameter("startTime", "numeric", 1.0, NA, NA, desc="Simulation time at which to initiate fires"),
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, desc="Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc="Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc="Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc="Interval between save events")),
  inputObjects=data.frame(objectName=c("vegMap", globals(sim)$burnStats),
                          objectClass=c("RasterLayer", "numeric"),
                          other=rep(NA_character_, 2L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("Fires",
                                        "FiresCumul",
                                        "FireSizeDistribution",
                                        globals(sim)$burnStats),
                           objectClass=c("RasterLayer", "RasterLayer",
                                         "gg", "numeric"),
                           other=rep(NA_character_, 4L), stringsAsFactors=FALSE)
))

### event functions
doEvent.fireSpreadLcc <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(sim, name="vegMap")

    # Clear global variable
    sim[[globals(sim)$burnStats]] <- numeric()

    # do stuff for this event
    sim <- fireSpreadLccInit(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, params(sim)$fireSpreadLcc$startTime, "fireSpreadLcc", "burn")
    sim <- scheduleEvent(sim, params(sim)$fireSpreadLcc$.saveInterval, "fireSpreadLcc", "save")
    sim <- scheduleEvent(sim, params(sim)$fireSpreadLcc$.plotInitialTime, "fireSpreadLcc", "plot.init")
  } else if (eventType=="burn") {
    # do stuff for this event
    sim <- fireSpreadLccBurn(sim)
    # schedule the next events
    sim <- scheduleEvent(sim, time(sim), "fireSpreadLcc", "stats") # do stats immediately following burn
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpreadLcc$returnInterval, "fireSpreadLcc", "burn")
  } else if (eventType=="stats") {
    # do stuff for this event
    sim <- fireSpreadLccStats(sim)

    # schedule the next event
    ## stats scheduling done by burn event
  } else if (eventType=="plot.init") {
    # do stuff for this event
    Plot(sim$FiresCumul,zero.color = "white", legendRange=0:sim$maxFiresCumul)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpreadLcc$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(sim$FiresCumul, zero.color="white", legendRange=0:sim$maxFiresCumul)

    if(length(sim[[globals(sim)$burnStats]])>0) {

      nPixelsBurned <- sim[[globals(sim)$burnStats]]

      binwidthRange <- pmax(1,diff(range(nPixelsBurned*6.25))/30)
      sim$FireSizeDistribution <- qplot(main="", nPixelsBurned*6.25, xlab="Hectares",
                                        binwidth=binwidthRange)
      sim$FireSizeDistribution <- sim$FireSizeDistribution +
        theme(axis.text.x=element_text(size=10, angle = 45, hjust = 1, colour="black"),
              axis.text.y=element_text(size=10, colour="black"),
              axis.title.x=element_text(size=12, colour="black"),
              axis.title.y=element_text(size=12, colour="black"))
      suppressMessages(Plot(sim$FireSizeDistribution))
    }

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpreadLcc$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType=="save") {
    # do stuff for this event
    sim <- saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + params(sim)$fireSpreadLcc$.saveInterval, "fireSpreadLcc", "save")
  } else {
    warning(paste("Undefined event type: \'", events(sim)[1, "eventType", with=FALSE],
                  "\' in module \'", events(sim)[1, "moduleName", with=FALSE],"\'", sep=""))
  }
  return(invisible(sim))
}

fireSpreadLccInit <- function(sim) {
  ### create burn map that tracks fire locations over time

  sim$maxFiresCumul <- 7 # used in legend scale

  sim$Fires <- raster(extent(sim$vegMap), ncol=ncol(sim$vegMap),
                  nrow=nrow(sim$vegMap), vals=0)
  #sim$Fires <- as(sim$Fires, "RasterLayerSparse")
  setColors(sim$Fires,n=params(sim)$fireSpreadLcc$nFires+1) <-
    c("#FFFFFF", rev(heat.colors(params(sim)$fireSpreadLcc$nFires)))
  sim$FiresCumul <- sim$Fires
  return(invisible(sim))
}

fireSpreadLccBurn <- function(sim) {
  #   assignGlobal("fireSpreadProb", reclassify(x=getGlobal("vegMap"),
  #                                             rcl=cbind(1:11,
  #                                                       c(0.225,0.225,0.21,0.15,0.15,0.18,0.1,0.1,0,0,0)*
  #                                                         params(sim)$fireSpreadLcc$drought)))
  fireSpreadProb <- reclassify(x=sim$vegMap,
                                  rcl=cbind(1:11,
                                            c(0.225,0.225,0.21,0.15,0.15,0.18,0.1,0.1,0,0,0)*
                                            params(sim)$fireSpreadLcc$drought))
  nFires <- rpois(1,params(sim)$fireSpreadLcc$nFires*params(sim)$fireSpreadLcc$drought)
  sim$Fires <- SpaDES::spread(fireSpreadProb,
                   loci=as.integer(sample(1:ncell(fireSpreadProb), nFires)),
                   spreadProb=fireSpreadProb,
                   persistance=params(sim)$fireSpreadLcc$persistprob,
                   mask=NULL,
                   maxSize=1e8,
                   directions=8,
                   iterations=params(sim)$fireSpreadLcc$its,
                   plot.it=FALSE,
                   mapID=TRUE)

  sim$Fires[is.na(sim$vegMap)] <- NA
  names(sim$Fires) <- "Fires"
  setColors(sim$Fires,n=nFires + 1) <- c("#FFFFFF", rev(heat.colors(nFires)))

  sim$FiresCumul[] <- sim$FiresCumul[] + (sim$Fires[]>0)
  setColors(sim$FiresCumul) <- c(colorRampPalette(c("orange", "darkred"))(sim$maxFiresCumul))

  return(invisible(sim))
}

fireSpreadLccStats <- function(sim) {
  npix <- sim[[globals(sim)$burnStats]]

  sim[[globals(sim)$burnStats]] <- c(npix, length(which(values(sim$Fires)>0)))

  return(invisible(sim))
}

