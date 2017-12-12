stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "forestSuccessionBeacons",
  description = "A basic forest succession module based on Canada Land Cover Classes 2005 and Beacons.",
  keywords = c("forest succession", "LCC05", "land cover classification 2005", "Beacons"),
  childModules = character(),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
    person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut")),
    person("Steve", "Cumming", email = "Steve.Cumming@sbf.ulaval.ca", role = c("aut"))),
  version = numeric_version("1.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c("2015-01-01", NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "forestSuccessionBeacons.Rmd"),
  reqdPkgs = list("ggplot2", "raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "Time interval between succession events"),
    defineParameter("startTime", "numeric", start(sim), NA, NA, desc = "Simulation time at which to initiate forest succession"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, desc = "Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc = "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc = "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc = "Interval between save events")
  ),
  inputObjects = data.frame(
    objectName = c("ageMap", "trajMapBeacons", "vegMapBeacons", "vegMap", "trajObj"),
    objectClass = c("RasterLayer", "RasterLayer", "RasterLayer", "RasterLayer", "matrix"),
    sourceURL = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    other = rep(NA_character_, 5L),
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = c("trajMap", "vegMap", "vegTypeDistribution"),
    objectClass = c("RasterLayer", "RasterLayer", "gg"),
    other = rep(NA_character_, 3L),
    stringsAsFactors = FALSE
  )
))

### event functions
doEvent.forestSuccessionBeacons <- function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
    # do stuff for this event
    sim <- forestSuccessionInit(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, params(sim)$forestSuccessionBeacons$startTime,
                         "forestSuccessionBeacons", "succession")
    sim <- scheduleEvent(sim, params(sim)$forestSuccessionBeacons$.plotInitialTime,
                         "forestSuccessionBeacons", "plot.init", .last())
  },
    succession = {
    # do stuff for this event
    sim <- forestSuccessionSuccession(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) +
                           params(sim)$forestSuccessionBeacons$returnInterval,
                         "forestSuccessionBeacons", "succession")
  },
    plot.init = {
    # do stuff for this event
    Plot(sim$vegMap)
    Plot(sim$trajMap)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) +
                           params(sim)$forestSuccessionBeacons$.plotInterval,
                         "forestSuccessionBeacons", "plot", .last())
  },
    plot = {
    # do stuff for this event
    Plot(sim$vegMap)

    # ggplot
    labelsShort <- character(max(levels(sim$vegMap)[[1]]$ID))
    labelsShort[levels(sim$vegMap)[[1]]$ID] <- sapply(
      strsplit(as.character(levels(sim$vegMap)[[1]]$Class), " "),
      function(x) paste(substr(x, 1, 3), collapse = "_")
    )
    veg <- data.frame(veg = sort(na.omit(getValues(sim$vegMap))))
    histColors <- getColors(sim$vegMap)$layer
    sim$vegTypeDistribution <- ggplot(veg, aes(factor(veg), fill = factor(veg)),
                                      xlab = "Vegetation Type") +
      geom_bar() +
      scale_fill_manual(values = histColors[as.numeric(as.character(unique(veg$veg)))]) +
      scale_x_discrete(breaks = 1:11, labels = labelsShort) +
      theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, colour = "black"),
            axis.text.y = element_text(size = 10, colour = "black"),
            axis.title.x = element_text(size = 12, colour = "black"),
            axis.title.y = element_text(size = 12, colour = "black"),
            legend.position = "none")
    Plot(sim$vegTypeDistribution)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) +
                           params(sim)$forestSuccessionBeacons$.plotInterval,
                         "forestSuccessionBeacons", "plot", .last())
  },

    warning(paste(
      "Undefined event type: \'", events(sim)[1,"eventType", with = FALSE],
      "\' in module \'", events(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )

  return(invisible(sim))
}

forestSuccessionInit <- function(sim) {
  sim$vegMap <- sim$vegMapBeacons
  sim$trajMap <- sim$trajMapBeacons
  return(invisible(sim))
}

forestSuccessionSuccession <- function(sim) {
  # assuming ageMap has zeros on it, this increases index to 1
  ageMap.v <- round(getValues(sim$ageMap)) + 1
  trajMap.v <- getValues(sim$trajMap)
  sim$vegMap <- setValues(sim$vegMap, sim$trajObj[cbind(ageMap.v, trajMap.v)])
  return(invisible(sim))
}

