stopifnot(packageVersion("SpaDES") >= "1.0.1")

defineModule(sim, list(
  name = "LCC2005",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = c(person(c("Alex", "M."), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = c("caribouMovementLcc", "cropReprojectLccAge", "fireSpreadLcc", "forestAge", "forestSuccessionBeacons", "LccToBeaconsReclassify"),
  version = numeric_version("1.0.3"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("LCC2005.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur")
  ),
  inputObjects = data.frame(
    objectName = NA_character_, objectClass = NA_character_, other = NA_character_, stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = NA_character_, objectClass = NA_character_, other = NA_character_, stringsAsFactors = FALSE)
))

### no other code is needed for this module group
