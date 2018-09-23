stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "LCC2005",
  description = "A set of modules simulating forest vegetation dynamics (succession and aging) based on Canada Land Cover Classes 2005.",
  keywords = c("forest", "succession", "vegetation dynamics", "LCC05", "land cover classification 2005"),
  authors = c(person(c("Alex", "M."), "Chubaty",
                     email = "alexander.chubaty@canada.ca",
                     role = c("aut", "cre"))),
  childModules = c(
    "caribouMovementLcc", "cropReprojectLccAge", "fireSpreadLcc",
    "forestAge", "forestSuccessionBeacons", "LccToBeaconsReclassify"
  ),
  version = list(LCC2005 = "1.1.2", caribouMovementLcc = "1.1.0", cropReprojectLccAge = "1.1.5",
                 fireSpreadLcc = "1.1.2", forestAge = "1.1.2", forestSuccessionBeacons = "1.1.1",
                 LccToBeaconsReclassify = "1.1.3"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("LCC2005.Rmd"),
  reqdPkgs = list(),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA,
                    "simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "simulation time interval when plot event should occur after the first"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "simulation time interval when save event should occur after the first")
  ),
  inputObjects = data.frame(
    objectName = NA_character_, objectClass = NA_character_,
    sourceURL = NA_character_, other = NA_character_, stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = NA_character_, objectClass = NA_character_,
    other = NA_character_, stringsAsFactors = FALSE)
))

### no other code is needed for this module group
