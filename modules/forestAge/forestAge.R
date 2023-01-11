defineModule(sim, list(
  name = "forestAge",
  description = "A basic forest age module based on Canada Land Cover Classes 2005.",
  keywords = c("forest age", "LCC05", "land cover classification 2005"),
  childModules = character(),
  authors = c(
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre")),
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person("Steve", "Cumming", email = "Steve.Cumming@sbf.ulaval.ca", role = c("aut"))),
  version = numeric_version("1.1.5"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "forestAge.Rmd"),
  reqdPkgs = list("raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "Time interval between aging events"),
    defineParameter("startTime", "numeric", start(sim) + 0.5, NA, NA, desc = "Simulation time at which to initiate forest aging"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, 0, desc = "Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc = "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc = "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc = "Interval between save events")
  ),
  inputObjects = data.frame(
    objectName = c("ageMapInit", "ageMap"),
    objectClass = c("RasterLayer", "RasterLayer"),
    sourceURL = c(
      #"ftp://ftp.daac.ornl.gov/data/nacp/NA_TreeAge//data/can_age04_1km.tif",
      "https://github.com/PredictiveEcology/SpaDES.Workshops/raw/master/can_age04_1km.tif",
      NA_character_
    ),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = "ageMap", objectClass = "RasterLayer", other = NA_character_,
    stringsAsFactors = FALSE)
))

### event functions
doEvent.forestAge <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ### check for object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- sim$forestAgeInit(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, params(sim)$forestAge$startTime, "forestAge", "age")
      sim <- scheduleEvent(sim, params(sim)$forestAge$.plotInitialTime, "forestAge", "plot.init")

    },
    age = {
      # do stuff for this event
      sim <- sim$forestAgeAge(sim)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) +
                             params(sim)$forestAge$returnInterval,
                           "forestAge", "age")
    },
    plot.init = {
      # do stuff for this event
      mod$plotTitle <- "Stand age"
      Plot(sim$ageMap, legendRange = c(0, 200), title = mod$plotTitle)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + params(sim)$forestAge$.plotInterval,
                           "forestAge", "plot", .last())
    }, plot = {
      # somewhat convoluted way to prevent repeated title creation
      needNewTitle <- quickPlot:::.quickPlotEnv$quickPlot2$curr@arr
      suppressMessages(aa <- Cache(function(...) {"out"}, needNewTitle, "ageMap")) #
      updateTitle <- if (attr(aa, ".Cache")$newCache) mod$plotTitle else ""

      Plot(sim$ageMap, legendRange = c(0, 200), title = updateTitle)

      # schedule the next event
      sim <- scheduleEvent(sim, time(sim) + params(sim)$forestAge$.plotInterval,
                           "forestAge", "plot", .last())
    },
    warning(paste(
      "Undefined event type: \'", events(sim)[1, "eventType", with = FALSE],
      "\' in module \'", events(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

forestAgeInit <- function(sim) {
  sim$ageMap <- sim$ageMapInit
  setColors(sim$ageMap, n = 201) <- colorRampPalette(c("LightGreen", "DarkGreen"))(50)

  return(invisible(sim))
}

forestAgeAge <- function(sim) {
  sim$ageMap <- setValues(sim$ageMap, pmin(200, getValues(sim$ageMap) +
                                             params(sim)$forestAge$returnInterval))
  if (exists("Fires", envir = envir(sim))) {
    sim$ageMap[sim$Fires>0] <- 0
  }
  setColors(sim$ageMap, n = 201) <- colorRampPalette(c("LightGreen", "darkgreen"))(50)

  return(invisible(sim))
}
