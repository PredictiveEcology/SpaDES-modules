###
### name:         forestSuccessionBeacons
###
### description:  A basic forest succession module based on Canada Land Cover Classes 2005 and Beacons.
###
### keywords:     forest succession; LCC05; land cover classification 2005; Beacons
###
### authors:      Eliot J. B. McIntire <Eliot.McIntire@NRCan.gc.ca>
###               Alex M. Chubaty <Alexander.Chubaty@NRCan.gc.ca>
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
### reqdPkgs:     raster; RColorBrewer
###
### parameters:   paramName: returnInterval
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
### inputObjects: objectName: trajMapBeacons
###               objectClass: RasterLayer
###               other: NA
###
###               objectName: vegMapBeacons
###               objectClass: RasterLayer
###               other: NA
###
### outputObjects: objectName: trajMap
###                objectClass: RasterLayer
###                other: NA
###
###                objectName: vegMap
###                objectClass: RasterLayer
###                other: NA
###
###                objectName: vegTypeDistribution
###                objectClass: gg
###                other: NA
###
### forestSuccessionBeacons module metadata
defineModule(sim, list(
  name="forestSuccessionBeacons",
  description="A basic forest succession module based on Canada Land Cover Classes 2005 and Beacons.",
  keywords=c("forest succession", "LCC05", "land cover classification 2005", "Beacons"),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.2.0"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c("2015-01-01", NA)),
  timestep=31557600,
  citation=list(),
  reqdPkgs=list("ggplot2", "raster", "RColorBrewer"),
  parameters=rbind(
    defineParameter("returnInterval", "numeric", 1.0),
    defineParameter("startTime", "numeric", 1.0),
    defineParameter(".plotInitialTime", "numeric", 1.0),
    defineParameter(".plotInterval", "numeric", 1),
    defineParameter(".saveInitialTime", "numeric", NA_real_),
    defineParameter(".saveInterval", "numeric", NA_real_)),
  inputObjects=data.frame(objectName=c("ageMap", "trajMapBeacons", "vegMapBeacons", "trajObj"),
                          objectClass=c("RasterLayer", "RasterLayer", "RasterLayer", "matrix"),
                          other=rep(NA_character_, 4L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("trajMap", "vegMap", "vegTypeDistribution"),
                           objectClass=c("RasterLayer", "RasterLayer", "gg"),
                           other=rep(NA_character_, 3L), stringsAsFactors=FALSE)
))

### event functions
doEvent.forestSuccessionBeacons <- function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)


    # do stuff for this event
    sim <- forestSuccessionInit(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simParams(sim)$forestSuccessionBeacons$startTime,
                         "forestSuccessionBeacons", "succession")
    sim <- scheduleEvent(sim, simParams(sim)$forestSuccessionBeacons$.saveInterval,
                         "forestSuccessionBeacons", "save")
    sim <- scheduleEvent(sim, simParams(sim)$forestSuccessionBeacons$.plotInitialTime,
                         "forestSuccessionBeacons", "plot.init")
  } else if (eventType=="succession") {
    # do stuff for this event
    sim <- forestSuccessionSuccession(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) +
                           simParams(sim)$forestSuccessionBeacons$returnInterval,
                         "forestSuccessionBeacons", "succession")
  } else if (eventType=="plot.init") {
    # do stuff for this event
    Plot(getGlobal("vegMap"))
    Plot(getGlobal("trajMap"))

    # schedule the next event
    sim <- scheduleEvent(sim, simCurrentTime(sim) +
                           simParams(sim)$forestSuccessionBeacons$.plotInterval,
                         "forestSuccessionBeacons", "plot")
  } else if (eventType=="plot") {
    # do stuff for this event
    Plot(getGlobal("vegMap"))

    # ggplot
    labelsShort <- sapply(strsplit(as.character(levels(vegMap)[[1]]$Class)," "),
                          function(x) paste(substr(x, 1, 3),collapse="_"))
    veg <- data.frame(veg=sort(na.omit(getValues(vegMap))))
    histColors <- getColors(vegMap)$layer
    vegTypeDistribution <- ggplot(veg, aes(factor(veg), fill=factor(veg)), xlab="Vegetation Type") +
      geom_bar() +
      scale_fill_manual(values=histColors[as.numeric(as.character(unique(veg$veg)))]) +
      scale_x_discrete(breaks=1:11, labels=labelsShort) +
      theme(axis.text.x=element_text(size=10, angle = 45, hjust = 1, colour="black"),
            axis.text.y=element_text(size=10, colour="black"),
            axis.title.x=element_text(size=12, colour="black"),
            axis.title.y=element_text(size=12, colour="black"),
            legend.position="none")
    assignGlobal("vegTypeDistribution")
    Plot(getGlobal("vegTypeDistribution"))

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
  assignGlobal("vegMap", vegMapBeacons)
  assignGlobal("trajMap", trajMapBeacons)
  return(invisible(sim))
}

forestSuccessionSuccession <- function(sim) {
  ageMap.v <- round(getValues(getGlobal("ageMap")))+1 #assuming ageMap has zeros on it, this increases index to 1
  trajMap.v <- getValues(getGlobal("trajMap"))
  vegMap <- setValues(vegMap,getGlobal("trajObj")[cbind(ageMap.v,trajMap.v)])
  assignGlobal("vegMap", vegMap)
  return(invisible(sim))
}

