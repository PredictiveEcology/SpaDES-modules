stopifnot(packageVersion("SpaDES") >= "1.3.1.9010")

defineModule(sim, list(
  name = "cropReprojectLccAge",
  description = paste(
    "A translator module.",
    "Crops and reprojects the land cover classification from 2005 to a smaller,",
    "cropped RasterLayer, defined by ext, and with new projection defined by newCRS"
  ),
  keywords = c("translator", "lcc05", "Land Cover Classification", "vegetation"),
  childModules = character(),
  authors = c(person(c("Eliot", "J","B"), "McIntire", email = "eliot.mcintire@canada.ca", role = c("aut", "cre"))),
  version = numeric_version("1.1.3"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = NA_character_,
  citation = list("citation.bib"),
  documentation = list("README.txt", "cropReprojectLccAge.Rmd"),
  reqdPkgs = list("archivist", "raster","rgeos", "parallel", "sp", "SpaDES"),
  parameters = rbind(
    defineParameter("useCache", "logical", TRUE, NA, NA,
                    "Should slow raster and sp functions use cached versions to speedup repeated calls"),
    defineParameter(".plotInitialTime", "numeric", NA_real_, NA, NA,
                    "Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA_real_, NA, NA,
                    "Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA,
                    "Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA,
                    "Interval between save events")),
  inputObjects = data.frame(
    objectName = c("lcc05", "age", "inputMapPolygon"),
    objectClass = c("RasterLayer", "RasterLayer", "SpatialPolygons"),
    sourceURL = c(NA_character_, NA_character_, NA_character_),
    other = rep(NA_character_, 3L), stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = c("vegMapLcc", "ageMapInit"),
    objectClass = c("RasterLayer", "RasterLayer"),
    other = rep(NA_character_, 2L), stringsAsFactors = FALSE)
))

doEvent.cropReprojectLccAge <- function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {

    # do stuff for this event
    sim <- sim$cropReprojectLccCacheFunctions(sim)
    sim <- sim$cropReprojectLccInit(sim)

    # schedule future event(s)
  } else {
    warning(paste(
      "Undefined event type: '", events(sim)[1, "eventType", with = FALSE],
      "' in module '", events(sim)[1, "moduleName", with = FALSE], "'", sep = ""
    ))
  }
  return(invisible(sim))
}

### template initilization
cropReprojectLccInit <- function(sim) {
  totalArea <- rgeos::gArea(inputMapPolygon) / 1e4
  if (totalArea > 100e6) {
    stop("In the current implementation, please select another, smaller polygon",
         " (less than 100 million hectares).")
  }
  #inputMapPolygon <- inputMapPolygon
  vegMapLcc2 <- sim$cropReprojectLccAge$crop(lcc05, inputMapPolygon)
  crs(vegMapLcc2) <- crs(sim$lcc05)

  sim$vegMapLcc <- sim$cropReprojectLccAge$mask(x = vegMapLcc2, mask = inputMapPolygon)
  setColors(sim$vegMapLcc, n = 256) <- getColors(sim$lcc05)[[1]] # mask removes colors!

  #if(ncell(sim$vegMapLcc)>5e5) beginCluster(min(parallel::detectCores(),6))

    # age will not run with projectRaster directly.
    # Instead, project the vegMap to age, then crop, then project back to vegMap.
    #vegMapLcc.crsAge <- sim$cropReprojectLccAge$projectRaster(sim$vegMapLcc, crs = crs(sim$age))
    maskLayer <- sim$cropReprojectLccAge$spTransform(sim$inputMapPolygon, CRSobj = crs(sim$age))
    age.crsAge2 <- sim$cropReprojectLccAge$crop(sim$age, maskLayer)
    age.crsAge <- sim$cropReprojectLccAge$mask(x = age.crsAge2, mask = maskLayer)
    sim$ageMapInit <- sim$cropReprojectLccAge$projectRaster(from = age.crsAge,
                                                            to = sim$vegMapLcc,
                                                            method = "ngb")

    if (sum(!is.na(getValues(sim$ageMapInit))) == 0) {
      stop("There are no age data provided with input age map")
    }
    if (sum(!is.na(getValues(sim$vegMapLcc))) == 0) {
      stop("There are no vegatation data provided with input vegatation map")
    }
    setColors(sim$ageMapInit) <- colorRampPalette(c("light green", "dark green"))(50)

  #endCluster()

  return(invisible(sim))
}

cropReprojectLccCacheFunctions <- function(sim) {
  # for slow functions, add cached versions. Then use sim$xxx() throughout module instead of xxx()

  if (params(sim)$cropReprojectLccAge$useCache) {
    # Step 1 - create a location for the cached data if it doesn't already exist
    sim$cacheLoc <- file.path(cachePath(sim), "cropReprojectLccAge") %>%
      checkPath(create = TRUE)
    if (!file.exists(file.path(sim$cacheLoc, "backpack.db"))) {
      createEmptyRepo(sim$cacheLoc)
    }

    # Step 2 - create a version of every function that is slow that includes the caching implicitly
    sim$cropReprojectLccAge$mask <- function(...) {
      SpaDES::cache(cacheRepo = sim$cacheLoc, FUN = raster::mask, ...)
    }
    sim$cropReprojectLccAge$crop <- function(...) {
      SpaDES::cache(cacheRepo = sim$cacheLoc, FUN = raster::crop, ...)
    }
    sim$cropReprojectLccAge$projectRaster <- function(...) {
      SpaDES::cache(cacheRepo = sim$cacheLoc, FUN = raster::projectRaster, ...)
    }
    sim$cropReprojectLccAge$spTransform <- function(...) {
      SpaDES::cache(cacheRepo = sim$cacheLoc, FUN = sp::spTransform,  ...)
    }
  } else {
    # Step 3 - create a non-caching version in case caching is not desired
    sim$cropReprojectLccAge$mask <- raster::mask
    sim$cropReprojectLccAge$crop <- raster::crop
    sim$cropReprojectLccAge$projectRaster <- raster::projectRaster
    sim$cropReprojectLccAge$spTransform <- sp::spTransform
  }

  return(invisible(sim))
}

### Inputs
.inputObjects <- function(sim) {
  if (is.null(sim$age) | is.null(sim$lcc05)) {
    checksums1 <- downloadData("LCC2005", file.path(modulePath(sim)))
    result1 <- checksums1[checksums1$expectedFile == "LCC2005_V1_4a.tif",]$result
    if (result1 != "OK" | is.na(result1)) {
      unzip(zipfile = file.path(modulePath(sim), "LccToBeaconsReclassify", "data", "LandCoverOfCanada2005_V1_4.zip"),
            files = "LCC2005_V1_4a.tif",
            exdir = file.path(modulePath(sim), "LccToBeaconsReclassify", "data"))
    }
    
    sim$age <- raster::raster(file.path(modulePath(sim), "forestAge", "data", "can_age04_1km.tif"))
    sim$lcc05 <- raster::raster(file.path(modulePath(sim), "LccToBeaconsReclassify", "data", "LCC2005_V1_4a.tif"))
  }

  lcc05CRS <- crs(sim$lcc05)
  #lcc05CRS <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +units=m +no_defs")

  # Random polygon
  areaKm2 <- 2000
  minX <- -1072250.2
  maxX <- minX + sqrt(areaKm2*1e6)
  minY <- 7438877 - 1.6e5
  maxY <- minY + sqrt(areaKm2*1e6)
  meanY <- mean(c(minY, maxY))

  # Add random noise to polygon
  #set.seed(5567913)
  xAdd <- round(runif(1, -5e5, 1.5e6))
  yAdd <- round(runif(1, 1e5, 5e5)) - xAdd/2
  nPoints <- 20
  betaPar <- 0.6
  X <- c(
    jitter(sort(rbeta(nPoints, betaPar, betaPar)*(maxX - minX) + minX)),
    jitter(sort(rbeta(nPoints, betaPar, betaPar)*(maxX - minX) + minX, decreasing = TRUE))
  )
  Y <- c(
    jitter(sort(rbeta(nPoints/2, betaPar, betaPar)*(maxY - meanY) + meanY)),
    jitter(sort(rbeta(nPoints, betaPar, betaPar)*(maxY - minY) + minY, decreasing = TRUE)),
    jitter(sort(rbeta(nPoints/2, betaPar, betaPar)*(meanY - minY) + minY))
  )

  inputMapPolygon <- cbind(X + xAdd, Y + yAdd) %>%
    Polygon() %>%
    list() %>%
    Polygons("s1") %>%
    list() %>%
    SpatialPolygons(1L)
  crs(inputMapPolygon) <- lcc05CRS

  sim <- sim$cropReprojectLccCacheFunctions(sim)

  inputMapPolygon <- sim$cropReprojectLccAge$spTransform(inputMapPolygon, CRSobj = lcc05CRS)

  sim$inputMapPolygon <- inputMapPolygon

  return(sim)
}
