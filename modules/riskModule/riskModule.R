# Everything in this file gets sourced during simInit, and all functions and objects
#  are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "riskModule",
  description = "This module creates a risk map, which starts green and turns yellow, orange and red as values go into risky places",
  keywords = c("risk"),
  authors = c(person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("0.0.2"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "riskModule.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter("riskInputRasterNames", "character", NA, NA, NA, "The names of the objects that will be used for risk"),
    defineParameter("calculateInitialTime", "numeric", NA, NA, NA, "This describes the simulation time when the first risk map should be calculated"),
    defineParameter("calculateInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between calculations of the risk map"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between subsequent plots"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")),
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
  inputObjects = data.frame(
    objectName = "FiresCumul", objectClass = "RasterLayer", sourceURL = "",
    other = NA_character_, stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = "riskMap", objectClass = "RasterLayer",
    other = NA_character_, stringsAsFactors = FALSE)
))

## event types
#   - type `init` is required for initiliazation

doEvent.riskModule <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$riskModuleInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, params(sim)$riskModule$calculateInitialTime, "riskModule", "calculate")
    sim <- scheduleEvent(sim, params(sim)$riskModule$.plotInitialTime, "riskModule", "plot")
    sim <- scheduleEvent(sim, params(sim)$riskModule$.saveInitialTime, "riskModule", "save")
  } else if (eventType == "calculate") {

    sim <- sim$riskModuleCalculate(sim)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$riskModule$calculateInterval, "riskModule", "calculate")

  } else if (eventType == "plot") {

    Plot(sim$riskMap, legendRange=c(1,4), cols=c("green", "yellow", "orange", "red"))
    sim <- scheduleEvent(sim, time(sim) + params(sim)$riskModule$.plotInterval, "riskModule", "plot")

  } else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                  "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initilization
riskModuleInit <- function(sim) {

  # # ! ----- EDIT BELOW ----- ! #
  sim$riskMap <- raster(sim[[params(sim)$riskModule$riskInputRasterNames[1]]]) %>%
    setValues(., 1) %>%
    mask(sim[[params(sim)$riskModule$riskInputRasterNames[1]]])

  levels(sim$riskMap) <- data.frame(
    ID = 1:4,
    Class = c("Very low", "Low", "Medium", "High")
  )

  setColors(sim$riskMap, 4) <- c("green", "yellow", "orange", "red")

  return(invisible(sim))
}

riskModuleCalculate <- function(sim) {
  #  Currently, the first is FiresCumul, the second is caribouRas
  #  so the following
  riskyVeryLow <- sim[[params(sim)$riskModule$riskInputRasterNames[1]]]>20
  riskyLow <- sim[[params(sim)$riskModule$riskInputRasterNames[1]]]<=20 &
    sim[[params(sim)$riskModule$riskInputRasterNames[2]]]>=2
  riskyMed <- sim[[params(sim)$riskModule$riskInputRasterNames[1]]]<=20 &
    sim[[params(sim)$riskModule$riskInputRasterNames[2]]]>=4
  riskyHigh <- sim[[params(sim)$riskModule$riskInputRasterNames[1]]]<=20 &
    sim[[params(sim)$riskModule$riskInputRasterNames[2]]]>=6

  sim$riskMap[riskyLow] <- 1
  sim$riskMap[riskyLow] <- 2
  sim$riskMap[riskyMed] <- 3
  sim$riskMap[riskyHigh] <- 4

  setColors(sim$riskMap, 4) <- c("green", "yellow", "orange", "red")

  return(invisible(sim))
}

### template for save events
riskModuleSave <- function(sim) {
  sim <- saveFiles(sim)
  return(invisible(sim))
}
