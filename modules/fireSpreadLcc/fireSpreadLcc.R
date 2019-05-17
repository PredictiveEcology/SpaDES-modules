stopifnot(packageVersion("SpaDES") >= "1.2.0.9006")

defineModule(sim, list(
  name = "fireSpreadLcc",
  description = "Simulate fire ignition and spread on a landscape, where spread probability varies according to percent pine. Fire size statistics are collected immediately after each burn event. Requires a global simulation parameter `.stackName` be set.",
  keywords = c("fire", "percolation model", "spread algorithm"),
  childModules = character(),
  authors = c(person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
              person("Steve", "Cumming", email = "Steve.Cumming@sbf.ulaval.ca", role = c("aut"))),
  version = numeric_version("1.1.4"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c("2005-01-01", NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "fireSpreadLcc.Rmd"),
  reqdPkgs = list("ggplot2", "methods", "raster", "RColorBrewer"),
  parameters = rbind(
    defineParameter("drought", "numeric", 1.00, 0.8, 1.4, desc = "An arbitrary index of drought, where 1 is 'normal', and greater than 1 is more dry"),
    defineParameter("nFires", "numeric", 10L, 0L, 100L, desc = "Number of fires to initiate at each returnInterval"),
    defineParameter("its", "numeric", 1e6, NA, NA, desc = "Maximum number of iterations for the spread algorithm"),
    defineParameter("burnStatsName", "character", "nPixelsBurned", NA, NA, desc = "Name for the burn statistics object"),
    defineParameter("persistprob", "numeric", 0.00, 0.00, 1.00, desc = "Probability that a burning cell will continue burning for 1 iteration"),
    defineParameter("returnInterval", "numeric", 1.0, NA, NA, desc = "Time interval between fire events"),
    defineParameter("startTime", "numeric", start(sim), NA, NA, desc = "Simulation time at which to initiate fires"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, desc = "Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, desc = "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc = "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc = "Interval between save events")),
  inputObjects = data.frame(
    objectName = c("vegMap", params(sim)$fireSpreadLcc$burnStatsName),
    objectClass = c("RasterLayer", "character"),
    sourceURL = c(NA_character_, NA_character_),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("Fires", "FiresCumul", "FireSizeDistribution", params(sim)$fireSpreadLcc$burnStatsName),
    objectClass = c("RasterLayer", "RasterLayer", "gg", "character"),
    other = rep(NA_character_, 4L), stringsAsFactors = FALSE)
))

### event functions
doEvent.fireSpreadLcc <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for object dependencies:
    ### (use `checkObject` or similar)
    checkObject(sim, name = "vegMap")

    # Clear global variable
    sim[[P(sim)$burnStatsName]] <- numeric()
    #sim[[P(sim)$burnStatsName]] <- numeric()

    # do stuff for this event
    sim <- sim$fireSpreadLccInit(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, P(sim)$startTime, "fireSpreadLcc", "burn")
    sim <- scheduleEvent(sim, P(sim)$.saveInterval, "fireSpreadLcc", "save")
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "fireSpreadLcc", "plot.init")
  } else if (eventType == "burn") {
    # do stuff for this event
    sim <- sim$fireSpreadLccBurn(sim)
    # schedule the next events
    sim <- scheduleEvent(sim, time(sim), "fireSpreadLcc", "stats") # do stats immediately following burn
    sim <- scheduleEvent(sim, time(sim) + P(sim)$returnInterval, "fireSpreadLcc", "burn")
  } else if (eventType == "stats") {
    # do stuff for this event
    sim <- sim$fireSpreadLccStats(sim)

    # schedule the next event
    ## stats scheduling done by burn event
  } else if (eventType == "plot.init") {
    # do stuff for this event
    mod$plotTitle <- "Cumulative number of\nfires in a pixel" # mod$ is for module-level objects
    Plot(sim$FiresCumul,zero.color = "white", legendRange = 0:sim$maxFiresCumul,
         cols=c("orange", "darkred"), title = mod$plotTitle)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType == "plot") {
    # somewhat convoluted way to prevent repeated title creation
    needNewTitle <- quickPlot:::.quickPlotEnv$quickPlot2$curr@arr
    suppressMessages(aa <- Cache(function(...) {"out"}, needNewTitle, "FiresCumul")) #
    updateTitle <- if (attr(aa, ".Cache")$newCache) mod$plotTitle else ""
    Plot(sim$FiresCumul,zero.color = "white", legendRange = 0:sim$maxFiresCumul,
         cols=c("orange", "darkred"), title = updateTitle)

    if (length(sim[[P(sim)$burnStatsName]]) > 0) {

      nPixelsBurned <- sim[[P(sim)$burnStatsName]]

      binwidthRange <- pmax(1,diff(range(nPixelsBurned*6.25))/30)
      sim$FireSizeDistribution <- qplot(main = "", nPixelsBurned*6.25,
                                        xlab = "Hectares", binwidth = binwidthRange)
      sim$FireSizeDistribution <- sim$FireSizeDistribution +
        theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, colour = "black"),
              axis.text.y = element_text(size = 10, colour = "black"),
              axis.title.x = element_text(size = 12, colour = "black"),
              axis.title.y = element_text(size = 12, colour = "black"))

      plotTitle2 <- "Fire size distribution"
      needNewTitle <- quickPlot:::.quickPlotEnv$quickPlot2$curr@arr
      suppressMessages(aa <- Cache(function(...) {"out"}, needNewTitle, "FiresDbn")) #
      updateTitle <- if (attr(aa, ".Cache")$newCache) plotTitle2 else ""

      suppressMessages(Plot(sim$FireSizeDistribution, title = updateTitle))
    }

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "fireSpreadLcc", "plot")
  } else if (eventType == "save") {
    # do stuff for this event
    sim <- saveFiles(sim)

    # schedule the next event
    sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "fireSpreadLcc", "save")
  } else {
    warning(paste("Undefined event type: \'",
                  events(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", events(sim)[1, "moduleName", with = FALSE],
                  "\'", sep = ""))
  }
  return(invisible(sim))
}

fireSpreadLccInit <- function(sim) {
  ### create burn map that tracks fire locations over time

  sim$maxFiresCumul <- 7 # used in legend scale

  sim$Fires <- raster(extent(sim$vegMap), ncol = ncol(sim$vegMap),
                      nrow = nrow(sim$vegMap), vals = 0) %>%
    mask(sim$vegMap)
  #sim$Fires <- as(sim$Fires, "RasterLayerSparse")
  setColors(sim$Fires, n = P(sim)$nFires + 1) <-
    c("#FFFFFF", rev(heat.colors(P(sim)$nFires)))
  sim$FiresCumul <- sim$Fires
  return(invisible(sim))
}

fireSpreadLccBurn <- function(sim) {
  fireSpreadProb <- reclassify(x = sim$vegMap,
                               rcl = cbind(1:11,
                                           c(0.225, 0.225, 0.21, 0.15, 0.15,
                                             0.18, 0.1, 0.1, 0, 0, 0)*
                                             P(sim)$drought))
  nFires <- rpois(1, P(sim)$nFires*P(sim)$drought)
  sim$Fires <- SpaDES.tools::spread(fireSpreadProb,
                                    loci = as.integer(sample(1:ncell(fireSpreadProb), nFires)),
                                    spreadProb = fireSpreadProb,
                                    persistance = P(sim)$persistprob,
                                    mask = NULL,
                                    maxSize = 1e8,
                                    directions = 8,
                                    iterations = P(sim)$its,
                                    plot.it = FALSE,
                                    id = TRUE)

  sim$Fires[is.na(sim$vegMap)] <- NA
  names(sim$Fires) <- "Fires"
  setColors(sim$Fires, n = nFires + 1) <- c("#FFFFFF", rev(heat.colors(nFires)))

  sim$FiresCumul[] <- sim$FiresCumul[] + (sim$Fires[] > 0)
  setColors(sim$FiresCumul) <- c(colorRampPalette(c("orange", "darkred"))(sim$maxFiresCumul))

  return(invisible(sim))
}

fireSpreadLccStats <- function(sim) {
  npix <- sim[[P(sim)$burnStatsName]]
  sim[[P(sim)$burnStatsName]] <- c(npix, length(which(values(sim$Fires) > 0)))
  return(invisible(sim))
}

