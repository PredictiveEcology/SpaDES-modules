stopifnot(packageVersion("SpaDES") >= "1.2.0.9009")

defineModule(sim, list(
  name = "test",
  description = "used for SpaDES package development testing",
  keywords = c("devlopment", "testing", "SpaDES package"),
  authors = c(person(c("Alex", "M"), "Chubaty",
                     email = "alexander.chubaty@canada.ca",
                     role = c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_, # e.g., "year,",
  citation = list("citation.bib"),
  documentation = list("README.txt", "test.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = c("DEM", "habitatQuality"), # DEM is smallest; habitatQuality is largest
    objectClass = c("RasterLayer", "RasterLayer"),
    sourceURL = c("https://github.com/PredictiveEcology/SpaDES/raw/master/inst/maps/DEM.tif",
                  "https://github.com/PredictiveEcology/SpaDES/raw/master/inst/maps/habitatQuality.tif"),
    other = NA_character_,
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))

## event types
#   - type `init` is required for initiliazation

doEvent.test = function(sim, eventTime, eventType, debug = FALSE) {
  # this is a stub function
  return(invisible(sim))
}
