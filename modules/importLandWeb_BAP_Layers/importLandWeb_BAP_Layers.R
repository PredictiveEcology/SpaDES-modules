
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "importLandWeb_BAP_Layers",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre")),
  childModules = character(0),
  version = numeric_version("1.3.1.9020"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "importLandWeb_BAP_Layers.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between save events"),
    defineParameter(".useCache", "numeric", TRUE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = "CanopyCover_1990", objectClass = "RasterLayer", 
                 desc = "Classified layer from Paul Pickell", sourceURL = "https://docs.google.com/uc?id=0BxZrk9psrK4naXNqMV9HcVVoeUE&export=download")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "SPP_1990", objectClass = "RasterLayer", desc = "Species classes according to Pickell et al document https://drive.google.com/open?id=0BxZrk9psrK4nc0J0MF9FaWRnSjg")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.importLandWeb_BAP_Layers = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- sim$importLandWeb_BAP_LayersInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "importLandWeb_BAP_Layers", "plot")
    sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "importLandWeb_BAP_Layers", "save")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    Plot(mySim$SPP_1990, visualSqueeze = 0.99)
    
    #Plot(objectFromModule) # uncomment this, replace with object to plot
    # schedule future event(s)

    # e.g.,
    #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "importLandWeb_BAP_Layers", "plot")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "save") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "importLandWeb_BAP_Layers", "save")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "event1") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "importLandWeb_BAP_Layers", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "event2") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, time(sim) + increment, "importLandWeb_BAP_Layers", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
importLandWeb_BAP_LayersInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  
  

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
importLandWeb_BAP_LayersSave <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
importLandWeb_BAP_LayersPlot <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  #Plot("object")

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event1
importLandWeb_BAP_LayersEvent1 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event1Test1 <- " this is test for event 1. " # for dummy unit test
  sim$event1Test2 <- 999 # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for your event2
importLandWeb_BAP_LayersEvent2 <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # THE NEXT TWO LINES ARE FOR DUMMY UNIT TESTS; CHANGE OR DELETE THEM.
  sim$event2Test1 <- " this is test for event 2. " # for dummy unit test
  sim$event2Test2 <- 777  # for dummy unit test


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects = function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create an named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can use 'sim$.userSuppliedObjNames' in their function below to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call. e.g.,
  # if (!('defaultColor' %in% sim$.userSuppliedObjNames)) {
  #  sim$defaultColor <- 'red'
  # }
  # ! ----- EDIT BELOW ----- ! #
  dataDir <- file.path(modulePath(sim), "importLandWeb_BAP_Layers", "data")
  
  allFiles <- c("CC_1990_100m.tif", "CC_1990_FILLED_100m.tif", "CC_2000_100m.tif", 
         "CC_2010_100m.tif", "HT_1990_100m.tif", "HT_1990_FILLED_100m.tif", 
         "HT_2000_100m.tif", "HT_2010_100m.tif", "SPP_1990_100m.tif", 
         "SPP_1990_100m.tif.aux.xml", "SPP_1990_FILLED_100m.tif", "SPP_1990_FILLED_100m.tif.aux.xml", 
         "SPP_2000_100m.tif", "SPP_2000_100m.tif.aux.xml", "SPP_2010_100m.tif", 
         "SPP_2010_100m.tif.aux.xml")
  haveAllFiles <- allFiles %in% dir(dataDir)
  
  #check <- checksums("importLandWeb_BAP_Layers", modulePath(sim))
  if(!all(haveAllFiles)) {
    
    message("Download file from browser. Save it to ", dataDir)
    wh <- which(unlist(lapply(sim@depends@dependencies, function(x) x@name=="importLandWeb_BAP_Layers")))
    browseURL(sim@depends@dependencies[[wh]]@inputObjects$sourceURL)
    readline("Press any key when file is downloaded")
  #downloadData("importLandWeb_BAP_Layers")
    untar(exdir = dataDir, #files = allFiles[!haveAllFiles], 
          file.path(dataDir, "2016-12-1 14.58.57-LANDWEB_DATA_LAYERS.tar.gz"))
  }
    
    
  origFiles <- dir(dataDir, pattern = "dat$", full.names = TRUE)
  
  args <- list(origFiles, function(i) {
    a <- SpaDES::rasterToMemory(i)
    if(grepl(basename(i), pattern="^SPP")) { 
      levels(a) <- data.frame(ID=c(11, 14, 22, 23, 26, 31, 32, 33, 34, 41, 42, 43, 44, 201), 
                              Names = c("Decid pure", "Decid-PiceGlau", "PiceMari pure", 
                                        "PiceMari-Pinus", "PiceMari-other", "Pinus-Decid",
                                        "Pinus-PiceMari", "Pinus pure", "Pinus-PiceGlau",
                                        "PiceGlau-Decid", "PiceGlau-PiceMari", "PiceGlau-Pinus",
                                        "PiceGlau pure", "Tie"))
      setColors(a) <- "Set3"
    } else {
      a[a[]>90] <- NA
      if(grepl(basename(i), pattern = "^CC")) {
        setColors(a) <- "YlGnBu"
      } else {
        setColors(a) <- "Greens"
      }
    }
    newFileName <- paste(c(paste(c(strsplit(basename(i), split = "_NAD83")[[1]][1]), 
                                 collapse = "_"), ".tif"), 
                         collapse = "")
    writeRaster(a, filename = file.path(dataDir, newFileName), overwrite = TRUE)
    return(invisible(NULL))
  })
  
  #if(params(sim)$importLandWeb_BAP_Layers$.useCache) {
    fun <- "Cache"
    args <- append(list(FUN = lapply), args)
  #} else {
  #  fun <- "lapply"
  #}
  do.call(fun, args)
  #Cache(lapply, origFiles, ...)
        
  sim$SPP_1990 <- Cache(rasterToMemory, file.path(dataDir, "SPP_1990_FILLED_100m.tif"))
  
  suppressWarnings(levels(sim$SPP_1990) <- data.frame(ID=c(11, 14, 22, 23, 26, 31, 32, 33, 34, 41, 42, 43, 44, 201), 
                          Names = c("Decid pure", "Decid-PiceGlau", "PiceMari pure", 
                                    "PiceMari-Pinus", "PiceMari-other", "Pinus-Decid",
                                    "Pinus-PiceMari", "Pinus pure", "Pinus-PiceGlau",
                                    "PiceGlau-Decid", "PiceGlau-PiceMari", "PiceGlau-Pinus",
                                    "PiceGlau pure", "Tie")) )
  setColors(sim$SPP_1990) <- "Set3"

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
### add additional events as needed by copy/pasting from above
