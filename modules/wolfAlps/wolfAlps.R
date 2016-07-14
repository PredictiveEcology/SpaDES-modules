
## Module metadata
defineModule(sim, list(
  name = "wolfAlps",
  description = "Movement model for wolves in the Italian Alps -- translation of the SELES model from Marucco and McIntire, 2010",
  keywords = c("wolf", "Alps", "movement"),
  authors = c(person("Sarah", "Bauduin", email="sarahbauduin@hotmail.fr", role=c("aut", "cre")),
              person("Eliot", "McIntire", email="eliot.mcintire@canada.ca", role=c("aut", "cre"))),
  childModules = character(),
  version = numeric_version("1.1.4.9011"),
  spatialExtent = raster::extent(rep(NA_real_, 4)), # raster::extent(raster(paste(inputDir, "/HabitatSuitability.asc", sep = ""))),
  timeframe = as.POSIXlt(c(NA, NA)), # as.POSIXlt(c(0, 14)),
  timeunit = "year", # e.g., "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "wolfAlps.Rmd"),
  reqdPkgs = list("NetLogoR", "SpaDES", "raster", "plyr", "data.table", "fpCompare", "testthat"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter(".plotInitialTime", "numeric", 2008, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", 2008, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter(".saveInterval", "numeric", 1, NA, NA, "This describes the simulation time at which the first save event should occur"),
    defineParameter("MeanNpups", "numeric", 3.387, NA, NA, "Mean number of pups per female per year"),
    defineParameter("SdNpups", "numeric", 1.210, NA, NA, "Standard deviation for the number of pups per female per year"),
    defineParameter("AdultMortalityRate", "numeric", 0.18, NA, NA, "Mortality rate per year for all wolves, except the 1 year old"),
    defineParameter("JuvMortalityRate", "numeric", 0.449, NA, NA, "Mortality rate per year for 1 year old wolves"),
    defineParameter("MeanPackSize", "numeric", 4.405, NA, NA, "Mean number of wolves in a pack"),
    defineParameter("SdPackSize", "numeric", 1.251, NA, NA, "Standard deviation for the number of wolves in a pack"),
    defineParameter("EndDispersal", "numeric", 0.98, NA, NA, "Dispersal time allowed to find a new territory otherwise all remaining dispersers die"),
    defineParameter("DispMortRatePerMove", "numeric", 0.0353, NA, NA, "Mortality rate per dispersal time step for dispersers"),
    defineParameter("CellWidth", "numeric", 1.25, NA, NA, "Cell size in km"),
    defineParameter("MoveStep", "numeric", 10.929, NA, NA, "Step size for dispersal events (probably in cell unit)"),
    defineParameter("sigma", "numeric", 21.802, NA, NA, "standard deviation (in degrees) of the Normal distribution to calculate angle probabilities for the correlated movement"),
    defineParameter("MeanPixelQuality", "numeric", 0.84, NA, NA, "Threshold of the cell suitability value to make the cell available as a potential next location for dispersing wolves"),
    defineParameter("PhaseTransitionLower", "numeric", 0.198, NA, NA, "Minimum amount of time within a year before a dispersing wolf starts trying to establish its own new territory"),
    defineParameter("MinPixelQuality", "numeric", 0.376, NA, NA, "Minimum suitability value for a cell to be incorporated in a territory"),
    defineParameter("MinPackQuality", "numeric", 89.288, NA, NA, "Minimum total suitability for a territory to be established"),
    defineParameter("PackArea", "numeric", 256, NA, NA, "Maximum number of cells for a territory to be established"),
    defineParameter("run.tests", "logical", FALSE, NA, NA, "Should tests be run"),
    defineParameter("tImmigration", "numeric", NA, NA, NA, "Time when an immigrant wolf arrives in the landscape"),
    defineParameter("locImmigrant", "matrix", cbind(NA, NA), cbind(NA, NA), cbind(NA, NA), "Patch location were the immigrant wolf arrives in the landscape"),
    defineParameter("nGenes", "numeric", NA, NA, NA, "Number of genes for each wolf"),
    defineParameter("nAlleles", "numeric", NA, NA, NA, "Number of different alleles for gene")
  ),
  inputObjects = data.frame(
    objectName = c("wolves2008", # wolves initial locations in 2008
                   "packs2008", # pack locations in 2008
                   "CMR", # CMR data
                   "HabitatSuitability"), # wolf habitat suitability for the Italian Alps
    objectClass = c("raster", "raster", "raster", "raster"),
    sourceURL = "",
    other = rep(NA_character_, 4),
    stringsAsFactors = FALSE
  ),
  outputObjects = data.frame(
    objectName = NA_character_,
    objectClass = NA_character_,
    other = NA_character_,
    stringsAsFactors = FALSE
  )
))


## Event types
doEvent.wolfAlps = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {

    sim <- sim$wolfAlpsInit(sim)

    sim <- scheduleEvent(sim, floor(params(sim)$wolfAlps$.saveInitialTime), "wolfAlps", "saveStart")
    sim <- scheduleEvent(sim, start(sim), "wolfAlps", "yearly")
    sim <- scheduleEvent(sim, params(sim)$wolfAlps$.plotInitialTime, "wolfAlps", "plot")
    sim <- scheduleEvent(sim, floor(params(sim)$wolfAlps$.saveInitialTime) + 1 - 0.001, "wolfAlps", "saveEnd")

    # Add immigration?
    sim <- scheduleEvent(sim, params(sim)$wolfAlps$tImmigration, "wolfAlps", "immigration")

  } else if (eventType == "plot") {

    sim <- sim$wolfAlpsPlot(sim)
    sim <- scheduleEvent(sim, time(sim, "year") + params(sim)$wolfAlps$.plotInterval, "wolfAlps", "plot")

  } else if (eventType == "saveStart") {

    sim <- sim$wolfAlpsSaveStatPack(sim) # number of individuals and size of territories
    sim <- sim$wolfAlpsSaveStatSim(sim) # pack numbers, population sizes and number of dead wolves

    sim <- scheduleEvent(sim, time(sim, "year") + round(params(sim)$wolfAlps$.saveInterval), "wolfAlps", "saveStart")

  } else if (eventType == "saveEnd") {

    sim <- sim$wolfAlpsDistDisp(sim) # dispersal distances
    sim <- sim$wolfAlpsTerr(sim) # packIDWorld map

    sim <- scheduleEvent(sim, time(sim, "year") + round(params(sim)$wolfAlps$.saveInterval), "wolfAlps", "saveEnd")

  } else if (eventType == "yearly") {

    sim <- sim$wolfAlpsDemography(sim) # age, mortality, create dispersing wolves and turn subordinates adults into alpha
    sim <- sim$wolfAlpsSaveStatInd(sim) # save wolves information after death but before reproduction
    sim <- sim$wolfAlpsReproduce(sim) # reproduction

    sim <- scheduleEvent(sim, time(sim, "year") + 0.001, "wolfAlps", "dispersal")

    sim <- scheduleEvent(sim, time(sim, "year") + 1, "wolfAlps", "yearly")

  } else if (eventType == "dispersal") {

    sim <- sim$wolfAlpsDispersal(sim) # dispersal movement
    sim <- sim$wolfAlpsEstablish(sim) # join a pack or build a new territory
    sim <- sim$wolfAlpsSaveTerrSize(sim) # save the size of the new created territories

    if(NLcount(NLwith(agents = sim$wolves, var = "dispersing", val = 1)) != 0){
      sim <- scheduleEvent(sim, time(sim, "year") + 0.01, "wolfAlps", "dispersal")
    }

  } else if (eventType == "immigration") {

    sim <- sim$wolfAlpsImmigration(sim)
    # No event scheduling as this happen only once

  } else {
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  }
  return(invisible(sim))
}


## Event functions

### Initialization
wolfAlpsInit <- function(sim) {

  # Load the inputs (map data)
  InitialWolves <- sim$wolves2008
  MapOfPacksID <- sim$packs2008
  CMR <- sim$CMR
  HabitatSuitability  <- sim$HabitatSuitability
  HabitatSuitability[HabitatSuitability < 0] <- 0

  # Create the world onto which the wolves move using the HabitatSuitability layer
  sim$suitabilityWorld <- createWorld(1, 436, 1, 296, data = values(HabitatSuitability) / 1000) # worldMatrix version
  # Create a rasterLayer version using world2raster to have the same coordinates as the worldMatrix
  sim$suitabilityRaster <- world2raster(sim$suitabilityWorld) # rasterLayer version needed for the cir() function (SpaDES)
  # Extract the value once and use the vectorized value for the suitability
  sim$suitabilityVal <- sim$suitabilityWorld[]
  suitabilityValGood <- sim$suitabilityVal
  suitabilityValGood[suitabilityValGood < params(sim)$wolfAlps$MeanPixelQuality] <- 0 # to be used in dispersal
  sim$suitabilityValGood <- suitabilityValGood

  # Create the wolves
  # Wolves information are extracted from the raster layer so they need to be scaled (same coordinates) like the suitabilityWorld (i.e., transformed into a worldMatrix)
  initialWolvesWorld <- createWorld(1, 436, 1, 296, data = values(InitialWolves)) # wolves initial locations
  packIDVal <- values(MapOfPacksID) # 0 and -1 are not packs
  packIDVal[packIDVal == -1] <- 0
  sim$packIDWorld <- createWorld(1, 436, 1, 296, data = packIDVal) # initial pack locations
  CMRWorld <- createWorld(1, 436, 1, 296, data = values(CMR)) # CMR data

  # Extract the wolves locations, packID and CMR data
  wolfLoc <- NLwith(world = initialWolvesWorld, agents = patches(initialWolvesWorld), val = 1:61) # 1:61 = wolfID
  wolfID <- of(world = initialWolvesWorld, agents = wolfLoc) # ID in the order of the wolfLoc
  packID <- of(world = sim$packIDWorld, agents = wolfLoc) # packID value for each wolfID
  wolfCMR <- of(world = CMRWorld, agents = wolfLoc) # CMR value for each wolfID

  # Create the wolves object
  colnames(wolfLoc) <- c("xcor", "ycor")
  wolves <- createTurtles(n = 61, coords = wolfLoc, breed = "wolf")
  # Reassign the id in wolves to match with the order of the raster-extracted variables
  wolves <- NLset(turtles = wolves, agents = wolves, var = "who", val = wolfID)
  wolves <- turtlesOwn(turtles = wolves, tVar = "packID", tVal = packID)
  wolves <- turtlesOwn(turtles = wolves, tVar = "oldPackID", tVal = packID)
  wolves <- turtlesOwn(turtles = wolves, tVar = "CMR", tVal = wolfCMR)
  wolves <- sortOn(agents = wolves, var = "who")
  # WolfType data from the SELES model
  wolfType <- c(15, 5, 15, 5, 15, 5, 15, 5, 2, 12, 11, 11, 1, 1, 15, 5, 1, 11, 2, 15, 5, 11, 1, 2, 1, 11, 5, 15, 5, 15, 2,
                12, 2, 1, 11, 1, 5, 15, 1, 11, 1, 5, 15, 11, 1, 11, 2, 5, 15, 12, 1, 11, 5, 15, 1, 11, 1, 2, 12, 5, 15)
  # From the wolfType data, derive sex, ageClass, alpha and dispersing variables
  sex <- mapvalues(wolfType, from = c(1,2,5,11,12,15), to = c(rep("F", 3), rep("M", 3)))
  wolves <- turtlesOwn(turtles = wolves, tVar = "sex", tVal = sex)
  ageClass <- mapvalues(wolfType, from = c(1,11,2,12,5,15), to = c(rep("juvenile", 4), rep("adult", 2)))
  wolves <- turtlesOwn(turtles = wolves, tVar = "ageClass", tVal = ageClass)
  alpha <- mapvalues(wolfType, from = c(1,11,2,12,5,15), to = c(rep(0, 4), rep(1, 2)))
  wolves <- turtlesOwn(turtles = wolves, tVar = "alpha", tVal = alpha)
  wolves <- turtlesOwn(turtles = wolves, tVar = "dispersing", tVal = 0)
  # Age data from the SELES model -- fixed to match the WolfType
  age <- c(3, 3, 3, 3, 3, 3, 3, 3, 0, 0, -1, -1, -1, -1, 3, 3, -1, -1, 0, 3, 3, -1, -1, 0, -1, -1, 3, 3, 3, 3, 0, 0, 0, -1,
           -1, -1, 3, 3, -1, -1, -1, 3, 3, -1, -1, -1, 0, 3, 3, 0, -1, -1, 3, 3, -1, -1, -1, 0, 0, 3, 3)
  wolves <- turtlesOwn(turtles = wolves, tVar = "age", tVal = age)

  # Add genetics?
  if(!is.na(params(sim)$wolfAlps$nGenes)){ # if the number of genes per wolf has been provided, add genetics
    for(numGenes in 1:params(sim)$wolfAlps$nGenes){
      wolves <- turtlesOwn(turtles = wolves, tVar = paste("G", numGenes, "_1", sep = ""),
                           tVal = sample(1:(params(sim)$wolfAlps$nAlleles[numGenes]), size = NLcount(wolves), replace = TRUE))
      wolves <- turtlesOwn(turtles = wolves, tVar = paste("G", numGenes, "_2", sep = ""),
                           tVal = sample(1:(params(sim)$wolfAlps$nAlleles[numGenes]), size = NLcount(wolves), replace = TRUE))
    }
  }

  sim$wolves <- wolves

  # Intialize the outputs
  sim$out_terrSize <- cbind(time = numeric(0), terrSize = numeric(0))
  sim$out_newTerrSize <- numeric(0)
  sim$out_numDeadDisp <- numeric(0)
  sim$out_numDeadJuv <- numeric(0)
  sim$out_numJuvTot <- numeric(0)
  sim$out_deaths <- cbind(time = numeric(0), age = numeric(0))
  sim$out_statPack <- cbind(time = numeric(0), packSize = numeric(0), packID = numeric(0), territorySize = numeric(0))
  sim$out_statInd <- cbind.data.frame(time = numeric(0), age = numeric(0), ageClass = character(0), dispersing = numeric(0),
                                      alpha = numeric(0), packID = numeric(0), stringsAsFactors = FALSE)
  sim$out_distDisp <- cbind(time = numeric(0), distDisp = numeric(0))
  sim$out_statSim <- cbind(time = numeric(0), numPack = numeric(0), numPack2A = numeric(0),
                           popSize = numeric(0), popSizeJuv = numeric(0),
                           numDied = numeric(0), numJuvDied = numeric(0),
                           numDispDied = numeric(0), numJuvTot = numeric(0))
  sim$out_terr <- list()
  sim$out_joinCreate <- cbind(time = start(sim, "year"):end(sim, "year"),
                              join = 0, create = 0)
  sim$out_dispersers <- cbind(time = start(sim, "year"):end(sim, "year"),
                              disperserStart = 0, disperserEnd = 0)
  sim$out_wolfPresence <- createWorld(1, 436, 1, 296, data = 0)
  # the alpha type map
  sim$packAlphaType <- sim$packIDWorld
  sim$packAlphaType[] <- 0


  return(invisible(sim))
}


### Save events
wolfAlpsSaveTerrSize <- function(sim) { # record the size (in cells) of the new created territories
  sim$out_terrSize <- rbind(sim$out_terrSize, cbind(time = time(sim, "year"), terrSize = sim$out_newTerrSize))
  sim$out_terrSize <- sim$out_terrSize[sim$out_terrSize[,2] != 0 ,]
  return(invisible(sim))
}

wolfAlpsSaveStatInd <- function(sim) { # record wolves information
  sim$out_statInd <- rbind(sim$out_statInd, cbind(time = time(sim, "year"),
                                                  age = of(agents = sim$wolves, var = "age"),
                                                  ageClass = of(agents = sim$wolves, var = "ageClass"),
                                                  dispersing = of(agents = sim$wolves, var = "dispersing"),
                                                  alpha = of(agents = sim$wolves, var = "alpha"),
                                                  packID = of(agents = sim$wolves, var = "packID")))
  return(invisible(sim))
}

wolfAlpsSaveStatPack <- function(sim) { # record the pack information (number of individuals and size of territories)
  packInfo <- of(agents = sim$wolves, var = "packID")
  packInfo <- packInfo[!is.na(packInfo)] # packID in the wolves
  packInfoSize <- table(packInfo) # number of wolves per pack
  packMapInfo <- sim$packIDWorld[]
  packMapInfo <- packMapInfo[packMapInfo != 0] # size (in cells) of all territories
  sim$out_statPack <- rbind(sim$out_statPack,
                            cbind(time = time(sim, "year"), packSize = as.numeric(packInfoSize), packID = as.numeric(names(packInfoSize)),
                                  territorySize = as.numeric(table(packMapInfo))))
  return(invisible(sim))
}

wolfAlpsSaveStatSim <- function(sim) { # record pack numbers, population sizes and number of dead wolves

  if(time(sim, "year") > start(sim, "year")){ # the record for time = start are built when initializing sim$out_statSim

    # Number of packs
    packInd <- unique(of(agents = sim$wolves, var = "packID"))
    countPackInd <- length(packInd[!is.na(packInd)]) # NA for dispersers

    # Number of packs with both alpha wolves in it
    alphaInd <- NLwith(agents = sim$wolves, var = "alpha", val = 1)
    alphaFemale <- NLwith(agents = alphaInd, var = "sex", val = "F")
    packAlphaFemale <- of(agents = alphaFemale, var = "packID")
    alphaMale <- NLwith(agents = alphaInd, var = "sex", val = "M")
    packAlphaMale <- of(agents = alphaMale, var = "packID")
    packReproduce <- intersect(packAlphaFemale, packAlphaMale) # packID where there is both a male and female alpha

    # Population sizes
    popSize <- NLcount(sim$wolves)
    popSizeJuv <- NLcount(NLwith(agents = sim$wolves, var = "ageClass", val = "juvenile"))

    # map of pack Alpha situation
    onlyMale <- packAlphaMale[!(packAlphaMale %in% packReproduce)]
    onlyFemale <- packAlphaFemale[!(packAlphaFemale %in% packReproduce)]

    sim$packAlphaType[sim$packIDWorld %in% packReproduce]  <- 3
    if(length(onlyFemale)) sim$packAlphaType[sim$packIDWorld %in% onlyFemale]  <- 2
    if(length(onlyMale)) sim$packAlphaType[sim$packIDWorld %in% onlyMale]  <- 1


    sim$out_statSim <- rbind(sim$out_statSim, cbind(time = time(sim, "year"), numPack = countPackInd, numPack2A = length(packReproduce),
                                                    popSize = popSize, popSizeJuv = popSizeJuv,
                                                    numDied = max(of(agents = sim$wolves, var = "who")) - popSize,
                                                    numJuvDied = sum(sim$out_numDeadJuv), numDispDied = sum(sim$out_numDeadDisp),
                                                    numJuvTot = sum(sim$out_numJuvTot)))
  }

  return(invisible(sim))
}

wolfAlpsDistDisp <- function(sim) { # record the dispersal distances

  endDisp <- of(agents = sim$wolves, var = c("who", "xcor", "ycor")) # wolves locations
  whoInter <- intersect(sim$startDisp[,"who"], endDisp[,"who"]) # who of disperser wolves that are still alive
  startLoc <- sim$startDisp[sim$startDisp[,"who"] %in% whoInter,, drop = FALSE] # starting locations of the alive dispersers
  startLoc <- startLoc[order(startLoc[,"who"]),,drop = FALSE]
  endLoc <- endDisp[endDisp[,"who"] %in% whoInter,, drop = FALSE] # ending locations of the alive dispersers
  endLoc <- endLoc[order(endLoc[,"who"]),,drop = FALSE]
  sim$out_dispersers[sim$out_dispersers[,"time"] == floor(time(sim, "year")), "disperserEnd"] <- NROW(endLoc) # how many dispersers ended
  distDisp <- NLdist(agents = startLoc[, c(2,3), drop = FALSE], agents2 = endLoc[, c(2,3), drop = FALSE]) # distance between the locations

  if(length(distDisp) != 0){
    sim$out_distDisp <- rbind(sim$out_distDisp,
                              cbind(time = time(sim, "year"), distDisp = (distDisp * params(sim)$wolfAlps$CellWidth)))
  }

  return(invisible(sim))
}

wolfAlpsTerr <- function(sim) { # record the packIDWorld map
  sim$out_terr[[floor(time(sim, "year")) + 1]] <- sim$packIDWorld
  return(invisible(sim))
}


### Plot events
wolfAlpsPlot <- function(sim) {
  wolvesSP <- turtles2spdf(sim$wolves)
  sim$packID <- world2raster(sim$packIDWorld)
  #numPacks <- data.table(sim$out_statPack)[,list(NumPacks=length(packSize)),by=time]

  initialTime <- time(sim, "year") == start(sim, "year")
  curTime <- NROW(sim$out_statSim)
  if(initialTime) {
    dev();
    clearPlot();
    arr <- c(2,3)
  } else {
    arr <- NULL
  }

  yr <- sim$out_statSim[,"time"]
  Plot(x = yr[curTime], y = sim$out_statSim[,"numPack"][curTime], xlab = "Year", axes = "L", ylim = c(0,100),
       ylab = "# (green = both alphas, blue = all)", xlim = c(start(sim), end(sim)), addTo = "numPacks", cex = 0.5,
       arr = arr, col = "blue", title = "Number of Packs")
  Plot(x = yr[curTime], y = sim$out_statSim[,"numPack2A"][curTime], addTo = "numPacks", cex = 0.5,
       col = "green")

  Plot(x = yr[curTime], y = sim$out_statSim[,"popSize"][curTime], xlab = "Year", axes = "L", ylim = c(0,250),
       ylab = "# (green = juveniles, blue = all)", xlim = c(start(sim), end(sim)), addTo = "numIndivs", cex = 0.5,
       col = "blue", title = "Population size")
  Plot(x = yr[curTime], y = sim$out_statSim[,"popSizeJuv"][curTime], addTo = "numIndivs", cex = 0.5,
       col = "green")

  Plot(sim$packID, zero.color = "transparent", title = "Pack ID", new=!initialTime)
  Plot(sim$suitabilityRaster, cols = grey(0:100/100), title = "Habitat suitability with \npack ID & wolves")
  Plot(wolvesSP, addTo = "sim$suitabilityRaster", cols = "red", size = 3)
  Plot(sim$packID, addTo = "sim$suitabilityRaster", zero.color = "transparent")

  sim$packAlphaTypeRas <- raster::as.factor(world2raster(sim$packAlphaType))
  suppressWarnings(levels(sim$packAlphaTypeRas) <- data.frame(ID=c(0:3), alpha = c("Neither","Male", "Female", "Both")))
  #if(!initialTime)
  Plot(sim$packAlphaTypeRas, zero.color = "transparent", title = "Alphas present",
       legendRange = c(0,3))

  return(invisible(sim))
}


### Events (reproduction)
wolfAlpsReproduce <- function(sim) {

  if(time(sim, "year") >= 1){

    # Identify the wolves that will reproduce (i.e., alpha pair in a pack)
    alphaInd <- NLwith(agents = sim$wolves, var = "alpha", val = 1)
    alphaFemale <- NLwith(agents = alphaInd, var = "sex", val = "F")
    packAlphaFemale <- of(agents = alphaFemale, var = "packID")
    alphaMale <- NLwith(agents = alphaInd, var = "sex", val = "M")
    packAlphaMale <- of(agents = alphaMale, var = "packID")

    if(params(sim)$wolfAlps$run.tests) {
      # There shouldn't be more than 1 alpha female and 1 alpha male in all packs
      expect_true(all(table(packAlphaFemale) <= 1))
      expect_true(all(table(packAlphaMale) <= 1))
    }

    packReproduce <- intersect(packAlphaFemale, packAlphaMale) # packID where there is both a male and female alpha
    femaleReproduce <- NLwith(agents = alphaFemale, var = "packID", val = packReproduce)
    if(NLcount(femaleReproduce) != 0){

      if(params(sim)$wolfAlps$run.tests) {
        numWolves <- NLcount(sim$wolves) # original number of wolves in the population before they reproduce
      }

      whoFemaleReproduce <- of(agents = femaleReproduce, var = "who")
      # Different number of pups per female
      nPups <- round(rnorm(n = length(whoFemaleReproduce), mean = params(sim)$wolfAlps$MeanNpups, sd = params(sim)$wolfAlps$SdNpups))
      nPups[nPups < 1] <- 1 # parameters estimated from the distribution conditional on having pups

      # Output
      sim$out_numJuvTot <- c(sim$out_numJuvTot, sum(nPups)) # append the number of pups born this year

      # Create pups in the wolves object
      sim$wolves <- hatch(turtles = sim$wolves, who = whoFemaleReproduce, n = nPups, breed = "newborn") # breed = "newborn" to recognize the pups in the wolves object
      newborn <- NLwith(agents = sim$wolves, var = "breed", val = "newborn")

      # Add genetics?
      if(!is.na(params(sim)$wolfAlps$nGenes)){ # if the number of genes per wolf has been provided, add genetics
        # Newborns inherited the female alleles
        genesPop <- colnames(sim$wolves@.Data)[17:ncol(sim$wolves@.Data)] # vector of gene names
        maleReproduce <- NLwith(agents = alphaMale, var = "packID", val = packReproduce) # find the fathers
        maleAlleles <- of(agents = maleReproduce, var = genesPop) # and their alleles
        maleAlleles <- maleAlleles[rep(seq_len(nrow(maleAlleles)), nPups),] # repeat the alleles the number of times the newborns they had

        # Select randomly one allele per gene per newborn
        alleleSel <- matrix(runif(min = 0, max = 1, n = (params(sim)$wolfAlps$nGenes * sum(nPups))),
                            ncol = params(sim)$wolfAlps$nGenes, nrow = sum(nPups))
        alleleChoice <- maleAlleles
        alleleChoice[, seq(from = 1, to = (params(sim)$wolfAlps$nGenes)*2, by = 2)] <- alleleSel > 0.5
        alleleChoice[, seq(from = 2, to = (params(sim)$wolfAlps$nGenes)*2, by = 2)] <- alleleSel <= 0.5

        femaleAlleles <- of(agents = newborn, var = genesPop) # retrieve the alleles given by the mother
        alleleFinal <- femaleAlleles
        alleleFinal[which(alleleChoice == 1)] <- maleAlleles[which(alleleChoice == 1)] # replace the femaleAlleles by the random half of the maleAlleles

        sim$wolves <- NLset(turtles = sim$wolves, agents = newborn, var = genesPop, val = alleleFinal)
      }

      # The newborns inherit all the parent (femaleReproduce) parameters except for the who numbers. Some of the inherited variables must be changed
      newbornData <- cbind.data.frame(heading = runif(n = NLcount(newborn), min = 0, max = 360),
                                      prevX = NA,
                                      prevY = NA,
                                      breed = "wolf",
                                      CMR = 0,
                                      sex = sample(c("F", "M"), size = NLcount(newborn), replace = TRUE), # female or male with p = 0.5
                                      ageClass = "juvenile",
                                      alpha = 0,
                                      dispersing = 0,
                                      age = 0)
      sim$wolves <- NLset(turtles = sim$wolves, agents = newborn, var = colnames(newbornData), val = newbornData) # update the data for the pups

      if(params(sim)$wolfAlps$run.tests){
        expect_equivalent(NLcount(sim$wolves), numWolves + sum(nPups))
      }

    }
  }

  return(invisible(sim))
}

### Events (age, mortality, create dispersing wolves and turn subordinates adults into alpha)
wolfAlpsDemography <- function(sim) {

  whoWolves <- of(agents = sim$wolves, var = "who")
  sim$startDisp <- cbind(who = numeric(0), xcor = numeric(), ycor = numeric(0)) # reset sim$startDisp
  for(indWolf in whoWolves){ # demography happens one wolf at the time

    # Aging
    ageWolf <- of(agents = turtle(sim$wolves, who = indWolf), var = "age")
    deadWolf <- FALSE

    if(params(sim)$wolfAlps$run.tests){
      expect_true(ageWolf <= 14) # no wolf should be older than 14
    }

    sim$wolves <- NLset(turtles = sim$wolves, agents = turtle(sim$wolves, who = indWolf), var = "age", val = ageWolf + 1) # get 1 year older
    # ageClass update if needed
    if(ageWolf + 1 == 3){
      sim$wolves <- NLset(turtles = sim$wolves, agents = turtle(sim$wolves, who = indWolf), var = "ageClass", val = "adult") # adult at 3 years old
    }

    # Mortality
    if(time(sim, "year") >= 2){ # no initial mortality at first

      # Four categories for mortality: wolves of age 1, alpha wolves, wolves less than 15 and wolves 15 or more
      if(ageWolf + 1 == 1){ # mortality of 1 year old wolves
        dieWolves1 <- runif(n = 1, min = 0, max = 1) <= params(sim)$wolfAlps$JuvMortalityRate

        if(dieWolves1){

          # Output
          sim$out_deaths <- rbind(sim$out_deaths, cbind(time = time(sim, "year"), age = ageWolf + 1))
          sim$out_numDeadJuv <- c(sim$out_numDeadJuv, 1)

          sim$wolves <- die(turtles = sim$wolves, who = of(agents = turtle(sim$wolves, who = indWolf), var = "who"))
          deadWolf <- TRUE
        }

      } else if((of(agents = turtle(sim$wolves, who = indWolf), var = "alpha") == 1) & (ageWolf + 1 < 15)){ # mortality of alpha wolves
        if(time(sim, "year") >= 4){ # time constraint for mortality of alpha
          dieWolvesAlpha <- runif(n = 1, min = 0, max = 1) <= params(sim)$wolfAlps$AdultMortalityRate

          if(dieWolvesAlpha){

            # Output
            sim$out_deaths <- rbind(sim$out_deaths, cbind(time = time(sim, "year"), age = ageWolf + 1))

            sim$wolves <- die(turtles = sim$wolves, who = of(agents = turtle(sim$wolves, who = indWolf), var = "who"))
            deadWolf <- TRUE
          }
        }

      } else if((ageWolf + 1 < 15) & (ageWolf + 1 != 1)){ # mortality of the other wolves
        dieWolvesOther <- runif(n = 1, min = 0, max = 1) <= params(sim)$wolfAlps$AdultMortalityRate

        if(dieWolvesOther){

          # Output
          sim$out_deaths <- rbind(sim$out_deaths, cbind(time = time(sim, "year"), age = ageWolf + 1))
          if(of(agents = turtle(sim$wolves, who = indWolf), var = "ageClass") == "juvenile"){
            sim$out_numDeadJuv <- c(sim$out_numDeadJuv, 1)
          }

          sim$wolves <- die(turtles = sim$wolves, who = of(agents = turtle(sim$wolves, who = indWolf), var = "who"))
          deadWolf <- TRUE
        }

      } else { # all wolves too old (15) die

        # Output
        sim$out_deaths <- rbind(sim$out_deaths, cbind(time = time(sim, "year"), age = ageWolf + 1))

        sim$wolves <- die(turtles = sim$wolves, who = of(agents = turtle(sim$wolves, who = indWolf), var = "who"))
        deadWolf <- TRUE
      }

      # Update the packIDWorld map if th pack disapeared because this wolf was the only one left in it
      packsInd <- unique(of(agents = sim$wolves, var = "packID"))
      packsInd <- packsInd[!is.na(packsInd)] # remaining packID among the wolves (packID = NA for dispersers)
      packsMap <- unique(of(world = sim$packIDWorld, agents = patches(sim$packIDWorld)))
      packsMap <- packsMap[packsMap != 0] # packID on the map (packID = 0 = no pack)
      emptyPack <- packsMap[which(!packsMap %in% packsInd)] # packID with no wolves left in it
      if(length(emptyPack) != 0){
        pemptyPack <- NLwith(world = sim$packIDWorld, agents = patches(sim$packIDWorld), val = emptyPack) # territory (patches) where packs disapeared
        sim$packIDWorld <- NLset(world = sim$packIDWorld, agents = pemptyPack, val = 0)
      }

      if(params(sim)$wolfAlps$run.tests){
        packsMap <- unique(of(world = sim$packIDWorld, agents = patches(sim$packIDWorld)))
        packsMap <- packsMap[packsMap != 0] # packID on the map (packID = 0 = no pack)
        packIndMap <- intersect(packsInd, packsMap)
        # There should be the same packID in the wolves object and on the packIDWorld map
        expect_equivalent(length(packsInd), length(packIndMap))
        expect_equivalent(length(packsMap), length(packIndMap))
      }
    } # end of mortality

    if(deadWolf == FALSE){ # if the wolf did not die

      # Identify if the wolf can be a potential disperser (i.e., juvenile of age 1 or 2 which is not already dispersing)
      wolfData <- inspect(turtles = sim$wolves, who = indWolf)
      potentialDisp <- wolfData[wolfData[,"ageClass"] == "juvenile" & wolfData[,"age"] %in% c(1,2) & wolfData[,"dispersing"] == 0 , , drop = FALSE]

      if(NROW(potentialDisp) > 0) { # if the wolf is a potential disperser

        packSizeMax <- rnorm(n = 1, mean = params(sim)$wolfAlps$MeanPackSize, sd = params(sim)$wolfAlps$SdPackSize) # generate the maximum number of wolves allowed in the pack
        packID <- of(agents = sim$wolves, var = "packID")
        actualPackSize <- table(packID)
        myPackSize <- actualPackSize[as.numeric(names(actualPackSize)) == potentialDisp[,"packID"]] # number of wolves in the pack of the potential disperser

        if(myPackSize > packSizeMax){ # if the number of wolves in the pack is larger than what is allowed, the wolf become disperser
          sim$wolves <- NLset(turtles = sim$wolves, agents = turtle(sim$wolves, who = indWolf), var = c("dispersing", "packID"),
                              val = cbind(dispersing = 1, packID = NA))

          # Update the packIDWorld map if this wolf which disperse was the only one left in the pack
          packWhichDisapeared <- myPackSize == 1
          if(packWhichDisapeared){
            pPackDisapeared <- NLwith(world = sim$packIDWorld, agents = patches(sim$packIDWorld), val = as.numeric(names(which(packWhichDisapeared)))) # territory (patches) where the pack was
            sim$packIDWorld <- NLset(world = sim$packIDWorld, agents = pPackDisapeared, val = 0)
          }

          # Record the disperser location to calculate its dispersal distance when it either joins a pack or creates a new territory
          sim$startDisp <- rbind(sim$startDisp, of(agents = turtle(sim$wolves, who = indWolf), var = c("who", "xcor", "ycor")))
          sim$out_dispersers[sim$out_dispersers[,"time"] == floor(time(sim, "year")), "disperserStart"] <- sim$out_dispersers[sim$out_dispersers[,"time"] == floor(time(sim, "year")), "disperserStart"] + 1 # how many dispersers started

        }
      }

      if(params(sim)$wolfAlps$run.tests){
        packsInd <- unique(of(agents = sim$wolves, var = "packID"))
        packsInd <- packsInd[!is.na(packsInd)] # packID among the wolves (packID = NA for dispersers)
        packsMap <- unique(of(world = sim$packIDWorld, agents = patches(sim$packIDWorld)))
        packsMap <- packsMap[packsMap != 0] # packID on the map (packID = 0 = no pack)
        packIndMap <- intersect(packsInd, packsMap)
        # There should be the same packID in the wolves object and on the packIDWorld map
        expect_equivalent(length(packsInd), length(packIndMap))
        expect_equivalent(length(packsMap), length(packIndMap))
      }

      # If the wolf is a subordinate adult and not already an alpha, it can replace a missing alpha in its pack
      if((of(agents = turtle(sim$wolves, who = indWolf), var = "ageClass") == "adult") & (of(agents = turtle(sim$wolves, who = indWolf), var = "alpha") == 0)){

        myPackID <- of(agents = turtle(sim$wolves, who = indWolf), var = "packID")
        otherWolves <- NLwith(agents = sim$wolves, var = "packID", val = myPackID) # other wolves in its pack
        otherAlpha <- NLwith(agents = otherWolves, var = "alpha", val = 1) # other alpha wolves in its pack

        if(NLcount(otherAlpha) == 1){ #if there's only one alpha in the pack
          if(of(agents = otherAlpha, var = "sex") != of(agents = turtle(sim$wolves, who = indWolf), var = "sex")){ # and it is of the opposite sex
            sim$wolves <- NLset(turtles = sim$wolves, agents = turtle(sim$wolves, who = indWolf), var = "alpha", val = 1) # become an alpha
          }
        }
      }

    } # end of if the wolf did not die
  } # end of the loop for all the individuals

  return(invisible(sim))
}

### Events (dispersal movement)
wolfAlpsDispersal <- function(sim) {

  dispersers <- NLwith(agents = sim$wolves, var = "dispersing", val = 1) # dispersing wolves
  whoDispersers <- of(agents = dispersers, var = "who")
  nonDispersers <- other(agents = sim$wolves, except = dispersers)

  # Dispersal movement
  if(NLcount(dispersers) != 0){ # if there are still dispersers not dead

    coords <- coordinates(dispersers)
    if(is.null(dim(coords))) dim(coords) <- c(1,2)
    noNextLocs <- rep(TRUE, NROW(coords))
    stepRep <- rep(params(sim)$wolfAlps$MoveStep, NROW(coords)) # step to move for the first dispersal trial to find empty patches
    # Remove as available locations, the patches where there are already individuals
    coor <- coordinates(sim$wolves)
    wolfInds <- cellFromPxcorPycor(sim$suitabilityWorld, coor[,"xcor"], coor[,"ycor"]) # cell number (= indices)

    while(any(noNextLocs)) { # as long as there are wolves with no available next locations, increase for them the step length for the dispersal movement
      # Grow rings between 1 and 1.5 step length centered on the dispersers locations
      colnames(coords) <- c("x", "y")
      nextLocs <- data.table(cir(landscape = sim$suitabilityRaster, coords = coords,
                                 maxRadius = stepRep * 1.5, minRadius = stepRep,
                                 includeBehavior = "excludePixels"))
      xy <- PxcorPycorFromCell(world = sim$suitabilityWorld, cellNum = nextLocs$indices) # identify the patches coordinates for the cells defined in the rings
      colnames(xy) <- c("x", "y")
      # And add the patches coordinates to the data.table
      data.table::set(nextLocs, , j = "x", xy[,"x"]) # faster than nextLocs[,x:=xy["x"]]
      data.table::set(nextLocs, , j = "y", xy[,"y"]) # faster than nextLocs[,y:=xy["y"]]

      # Remove as available next locations, the ones where there are already wolves on it
      data.table::set(nextLocs, , j = "empty", 1) # empty = 1, it is empty
      nextLocs[nextLocs$indices %in% wolfInds, empty:=0] # empty = 0, there is a wolf on this cell

      noNextLocs <- nextLocs[,sum(empty == 1) == 0, by = id]$V1 # are all the patches occupied in the rings?
      stepRep[noNextLocs] <- stepRep + stepRep # double the step length for the next trial
    }

    # Now each wolf has its subset of cells as potential next locations for dispersal
    # Probability of going to the potential next locations regarding their directions
    headDispersers <- of(agents = dispersers, var = "heading")
    nextLocs[,nextAngle:={
      dnorm(mean = 0, sd = params(sim)$wolfAlps$sigma, # calculate the probability using the Normal distribution of ...
            subHeadings(angle1 = headDispersers[id], # ... the rotation of each wolf's heading to ...
                        angle2 = towards(agents = turtle(sim$wolves, who = whoDispersers[id]), # ... the direction towards each of its next potential locations
                                         agents2 = cbind(x = x, y = y))
            )
      )}, by = id] # data.table use of by = id, so each of the above happens within each id

    # Probability of going to the potential next locations regarding habitat suitability
    data.table::set(nextLocs,,"suitabilityValGood", sim$suitabilityValGood[nextLocs$indices])

    # Probability of going to the potential next locations regarding the directions, habitat suitability and other wolves presence
    data.table::set(nextLocs,,"prob", nextLocs$nextAngle * nextLocs$suitabilityValGood * nextLocs$empty)
    probLoc <- runif(n = NLcount(dispersers), min = 0, max = 1)
    setkeyv(nextLocs, c("id"))
    # Selected next potential locations, based on probLoc
    nextLocs <- nextLocs[,.SD[findInterval(probLoc[id], cumsum(prob/sum(prob)))+1],
                         by = id, .SDcols = c("x", "y", "prob")]
    selectedLocID <- as.matrix(nextLocs)[,c(2,3,1), drop = FALSE]

    # If some wolves don't have a next location because all their available cell to move had their probability = 0
    if(any(is.na(nextLocs))){
      whNA <- which(is.na(nextLocs$x))
      # Put their actual locations as their next location
      newLoc <- patchHere(world = sim$suitabilityWorld, turtles = dispersers[whNA,])
      selectedLocID <- rbind(selectedLocID,
                             cbind(x = newLoc[,1], y = newLoc[,2], id = nextLocs$id[whNA]))
      selectedLocID <- selectedLocID[!is.na(selectedLocID[,1]),]
    }

    # Move the wolves to the selected locations
    selectedLoc <- selectedLocID[order(selectedLocID[,"id"]), c(1,2), drop = FALSE] # order the destination by the dispersers

    if(params(sim)$wolfAlps$run.tests) {
      expect_true(length(selectedLoc[is.na(selectedLoc[,1]),1]) == 0) # there shouldn't be NA in the next locations
      expect_true(NROW(selectedLoc) == NLcount(dispersers)) # each wolf must have a destination
      dispersersBeforeMove <- dispersers
    }

    dispersers <- face(turtles = dispersers, agents2 = selectedLoc) # headings
    dispersers <- moveTo(turtles = dispersers, agents = selectedLoc) # locations
    # Update the dispersers inside the wolves object
    sim$wolves <- rbind(dispersers, nonDispersers)

    # Record the dispersing wolves new locations
    locDispersers <- patchHere(world = sim$out_wolfPresence, turtles = dispersers) # patches were the dispersing wolves are
    pValLocDisp <- of(world = sim$out_wolfPresence, agents = locDispersers) # values =  number of dispersing wolves visits so far
    sim$out_wolfPresence <- NLset(world = sim$out_wolfPresence, agents = locDispersers, val = pValLocDisp + 1) # record the dispersing wolves new locations

    if(params(sim)$wolfAlps$run.tests){
      dispersersUpdated <- NLwith(agents = sim$wolves, var = "dispersing", val = 1)
      distMoved <- NLdist(agents = dispersersBeforeMove, agents2 = dispersersUpdated)
      expect_true(all(distMoved > params(sim)$wolfAlps$MoveStep )) # all dispersers should have at least move of a MoveStep distance
    }

  }

  return(invisible(sim))
}

### Events (join a pack or build a new territory)
wolfAlpsEstablish <- function(sim) {

  dispersers <- sim$wolves[sim$wolves$dispersing == 1,]
  numDispersers <- NLcount(dispersers) # number of dispersers
  nonDispersers <- other(agents = sim$wolves, except = dispersers)
  oldPacks <- unique(of(agents = sim$wolves, var = "packID"))
  sim$out_newTerrSize <- 0
  pWolves <- patchHere(world = sim$suitabilityWorld, turtles = sim$wolves) # where are located the wolves

  if(numDispersers != 0){ # if there are still dispersers alive

    if(numDispersers == 1){
      seqDispersers <- 1
    } else {
      # Shuffle the sequence of the dispersers for randomness as they join pack or create territories one wolf at the time
      seqDispersers <- sample(1:numDispersers, size = numDispersers)
    }

    for(xDisp in seqDispersers){ # one wolf at the time

      disperserLoc <- patchHere(world = sim$suitabilityWorld, turtles = dispersers[xDisp,]) # disperser wolf location
      otherPWolves <- other(agents = pWolves, except = disperserLoc) # patches where are located all the wolves, except the dispersing wolf

      # Define which pack can be joined (has only one alpha) and the one that cannot (either no alpha at all or two alpha)
      existingPack <- unique(of(agents = sim$wolves, var = "packID")) # existing packID in the wolves
      wolvesAlpha <- sim$wolves[sim$wolves$alpha == 1,]
      wolvesAlphaPackID <- table(of(agents = wolvesAlpha, var = "packID"))
      packCanJoin <- as.numeric(names(wolvesAlphaPackID[wolvesAlphaPackID == 1])) # packID that have only 1 alpha
      packCannotJoin <- existingPack[!existingPack %in% packCanJoin]
      packCannotJoin <- packCannotJoin[!is.na(packCannotJoin)]

      # In the suitabilityVal vector (suitability values for the landscape), put 0 in the places where disperser cannot go (i.e., where there are packs they cannot join) and where there is already a wolf on it
      suitabilityValUpdated <- sim$suitabilityVal # needs to keep a clean copy of suitabilityVal to update each time
      suitabilityValUpdated[cellFromPxcorPycor(world = sim$suitabilityWorld, pxcor = otherPWolves[,1], pycor = otherPWolves[,2])] <- 0 # remove where there already are wolves
      suitabilityValUpdated[sim$packIDWorld[] %in% packCannotJoin] <- 0

      # Grow a territory
      # Create circles around the disperserLoc, up to a maxRadius of 14/(CellWidth^2) -> taken from SELES code
      colnames(disperserLoc) <- c("x", "y")
      possTerr <- cir(coords = disperserLoc, landscape = sim$suitabilityRaster, allowOverlap = FALSE, returnDistances = TRUE,
                      maxRadius = 14/(params(sim)$wolfAlps$CellWidth^2), minRadius = 0, includeBehavior = "includePixels")

      # Remove individual cells that don't qualify as territories based on suitability and pack presence that cannot be joined
      possTerr <- cbind(possTerr, newTerrSuitability2 = suitabilityValUpdated[possTerr[,"indices", drop = FALSE]]) # suitabilityValUpdated already contain the suitabilityVal
      notGoodEnough <- possTerr[,"newTerrSuitability2"] < params(sim)$wolfAlps$MinPixelQuality # which cells don't have a good enough quality
      possTerr <- possTerr[!notGoodEnough, , drop = FALSE] # remove those from the territory

      if(NROW(possTerr) > 0) {

        # Sort the cells in the territory by distance and keep only the closest ones to form a pack smaller than the PackArea allowed
        possTerr <- possTerr[order(possTerr[,"dists"]), , drop = FALSE][1:(min(params(sim)$wolfAlps$PackArea, NROW(possTerr))), , drop = FALSE]

        cellTerr <- sim$packIDWorld@pCoords[possTerr[,"indices"], , drop = FALSE] # identify the cells composing this territory
        otherPack <- of(world = sim$packIDWorld, agents = cellTerr) # and the pack already present that may be on this territory

        joined <- FALSE # the disperser has not joined yet a pack

        if(sum(otherPack) != 0){ # if there are packs already established overlapping the built territory

          otherPackID <- unique(otherPack) # which are the packID overlapping
          otherPackID <- otherPackID[otherPackID != 0] # 0 = no pack

          if(length(otherPackID) > 1){ # if there are more than one pack overlapping the territory, rank them by distance
            pOtherPacks <- PxcorPycorFromCell(world = sim$suitabilityWorld,
                                              cellNum = possTerr[otherPack != 0,"indices"]) # cells overlapping the packs
            valOtherPacks <- otherPack[otherPack != 0] # packID of the cells overlapping
            distOtherPacks <- NLdist(agents = dispersers[xDisp,], agents2 = pOtherPacks) # distance between the dispersers and each of these cells overlapping packs
            otherPackID <- unique(valOtherPacks[order(distOtherPacks)]) # rank the packID in an increasing distance order
          }

          for(eachPack in otherPackID){ # try to join each pack overlapping the territory in a increasing distance order

            if(joined == FALSE){ # if the disperser has not joined yet a pack
              wolvesIn <- NLwith(agents = sim$wolves, var = "packID", val = eachPack) # wolves belonging to the pack the disperser try to join
              wolvesInAlpha <- wolvesIn[wolvesIn$alpha == 1,] # alpha in this pack
              if(NLcount(wolvesInAlpha) == 1){ # if there is only one alpha of the opposite sex
                if(of(agents = wolvesInAlpha, var = "sex") != of(agents = dispersers[xDisp,], var = "sex")){ # and if it is of the opposite sex of the disperser, then join the pack

                  wolfDisperser <- moveTo(turtles = dispersers[xDisp,], agents = wolvesInAlpha) # move to the alpha wolf location
                  sim$wolves <- rbind(wolfDisperser, other(agents =  sim$wolves, except = wolfDisperser)) # update the location of the disperser wolf in the wolves object
                  # When a disperser joins a pack, it becomes alpha, adult, non disperser and take the packID
                  sim$wolves <- NLset(turtles = sim$wolves, agents = wolfDisperser, var = c("alpha", "ageClass", "dispersing", "packID", "oldPackID"),
                                      val = cbind.data.frame(alpha = 1, ageClass = "adult", dispersing = 0, packID = eachPack, oldPackID = eachPack))
                  sim$out_joinCreate[sim$out_joinCreate[,"time"] == floor(time(sim, "year")), "join"] <- sim$out_joinCreate[sim$out_joinCreate[,"time"] == floor(time(sim, "year")), "join"] + 1 # one more wolf joined a pack
                  joined <- TRUE
                }
              }
            }
          }
        }

        if(joined == FALSE){ # if there are no pack already established overlapping the built territory or if the disperser could not join the packs overlapping the built territory
          if(time(sim, "year") - floor(time(sim, "year")) >=
             runif(n = 1, min = params(sim)$wolfAlps$PhaseTransitionLower, max = 1)){ # behavioral transition parameter (min time required before creating a new territory)

            # Remove individual cells that don't qualify as territories based on suitability and all packs presence
            suitabilityValUpdated <- sim$suitabilityVal
            suitabilityValUpdated[cellFromPxcorPycor(world = sim$suitabilityWorld, pxcor = otherPWolves[,1], pycor = otherPWolves[,2])] <- 0 # remove where there already are other wolves
            suitabilityValUpdated[sim$packIDWorld[] %in% existingPack[!is.na(existingPack)]] <- 0 # remove all cells already in established territories
            suitabilityValUpdated[suitabilityValUpdated < params(sim)$wolfAlps$MinPixelQuality] <- 0 # remove cells which don't have a good enough quality
            suitabilityValUpdated2 <- suitabilityValUpdated
            suitabilityValUpdated2[suitabilityValUpdated2 != 0] <- 1 # all the other cells can have a territory on it
            suitabilityValUpdatedRaster <- setValues(sim$suitabilityRaster, suitabilityValUpdated2)

            # The dispersing wolf cannot join a pack, it will spread a contiguous territory avoiding all established territories
            stopRuleSuitability <- function(landscape){sum(landscape) > params(sim)$wolfAlps$MinPackQuality} # rule to stop territories from expanding when they reached max suitability
            possTerr <- spread(landscape = sim$suitabilityRaster,
                               loci = cellFromPxcorPycor(world = sim$suitabilityWorld, pxcor = disperserLoc[,1], pycor = disperserLoc[,2]),
                               spreadProb = suitabilityValUpdatedRaster, maxSize = params(sim)$wolfAlps$PackArea,
                               returnIndices = TRUE, circle = TRUE, stopRuleBehavior = "includePixel",
                               stopRule = stopRuleSuitability)
            possTerr <- possTerr[possTerr$active == FALSE,] # to keep the number of cells below or equal to maxSize

            if(params(sim)$wolfAlps$run.tests){
              expect_true(nrow(possTerr) <= 256)
            }

            # Decide if the disperser can establish its territory based on its suitability
            if(sum(suitabilityValUpdated[possTerr$indices]) > params(sim)$wolfAlps$MinPackQuality){ # if the total suitability is good enough then create a new territory

              # Cells composing the territory
              cellTerr <- sim$packIDWorld@pCoords[possTerr$indices, , drop = FALSE]

              # When a disperser create a pack, it becomes alpha, adult, non disperser and obtain a new packID not already used in the population
              sim$wolves <- NLset(turtles = sim$wolves, agents = dispersers[xDisp,], var = c("packID", "oldPackID", "dispersing", "alpha", "ageClass"),
                                          val = cbind.data.frame(packID = (max(existingPack, na.rm = TRUE) + 1), oldPackID = (max(existingPack, na.rm = TRUE) + 1), dispersing = 0, alpha = 1, ageClass = "adult"))
              sim$packIDWorld <- NLset(world = sim$packIDWorld, agents = cellTerr, val = (max(existingPack, na.rm = TRUE) + 1)) # update the packIDWorld map with the new created territory
              sim$out_joinCreate[sim$out_joinCreate[,"time"] == floor(time(sim, "year")), "create"] <- sim$out_joinCreate[sim$out_joinCreate[,"time"] == floor(time(sim, "year")), "create"] + 1 # one more wolf created a pack
            }
          }
        }
      } # end of if(NROW(possTerr) > 0)
    } # end of the loop for all the dispersers

    # Size of the new created packs (not the joined packs)
    newPackCreated <- sim$packIDWorld[!(sim$packIDWorld %in% c(oldPacks,0))]
    if(length(newPackCreated) != 0){
      sim$out_newTerrSize <- tapply(newPackCreated, newPackCreated, length)
    }

    if(params(sim)$wolfAlps$run.tests){
      # There shouldn't be more than 2 alpha (1 female and 1 male) per pack
      alphaWolves <- NLwith(agents = sim$wolves, var = "alpha", val = 1)
      wolvesData <- of(agents = alphaWolves, var = c("sex", "packID"))
      expect_true(all(tapply(wolvesData[,"sex"], wolvesData[,"packID"], length) <= 2))
      expect_true(all(tapply(wolvesData[,"sex"], wolvesData[,"packID"], function(x){length(which(x == "F"))}) <= 1))
      expect_true(all(tapply(wolvesData[,"sex"], wolvesData[,"packID"], function(x){length(which(x == "M"))}) <= 1))

      expect_true(all(sim$out_newTerrSize <= params(sim)$wolfAlps$PackArea)) # new territories should respect the maximum area condition
      packIDWorldVal <- sim$packIDWorld[]
      cellNumNewPack <- which(packIDWorldVal %in% unique(newPackCreated))
      newPackSuitability <- of(world = sim$suitabilityWorld,
                               agents = PxcorPycorFromCell(world = sim$suitabilityWorld, cellNum = cellNumNewPack))
      newPackCreated <- of(world = sim$packIDWorld,
                           agents = PxcorPycorFromCell(world = sim$suitabilityWorld, cellNum = cellNumNewPack))
      newPackMinSuitability <- tapply(newPackSuitability, newPackCreated, min)
      expect_true(all(newPackMinSuitability >= params(sim)$wolfAlps$MinPixelQuality)) # minimum cell suitability condition
      newPackTotalSuitability <- tapply(newPackSuitability, newPackCreated, sum)
      expect_true(all(newPackTotalSuitability > params(sim)$wolfAlps$MinPackQuality)) # total suitability in the territory condition
    }

    # Dispersal mortality for the remaining dispersers who haven't found a pack to join or a territory to create
    stillDispersing <- NLwith(agents = sim$wolves, var = "dispersing", val = 1) # wolves have been updated
    allNonDispersers <- other(agents = sim$wolves, except = stillDispersing)
    whoStillDispersing <- of(agents = stillDispersing, var = "who")
    numStillDispersing <- NLcount(stillDispersing)

    if(time(sim, "year") - floor(time(sim, "year")) < params(sim)$wolfAlps$EndDispersal){ # if wolves can still disperse
      inTerr <- patchHere(world = sim$packIDWorld, turtles = stillDispersing) # in which territories are the remaining dispersers
      packID <- of(world = sim$packIDWorld, agents = inTerr)
      whoStillDispersing <- whoStillDispersing[of(agents = stillDispersing, var = "oldPackID") != packID] # only the wolves not in their territories can die
      dieDispersers <- runif(n = length(whoStillDispersing), min = 0, max = 1) < params(sim)$wolfAlps$DispMortRatePerMove # they die according to DispMortRatePerMove at each time step
    } else {
      dieDispersers <- 1:numStillDispersing # if it's too late, all remaining dispersers die
    }

    if(length(whoStillDispersing[dieDispersers]) != 0 ){

      sim$out_deaths <- rbind(sim$out_deaths, cbind(time = time(sim, "year"), age = of(agents = turtle(sim$wolves, who = whoStillDispersing[dieDispersers]), var = "age")))

      stillDispersing <- die(turtles = stillDispersing, who = whoStillDispersing[dieDispersers])
      sim$wolves <- rbind(stillDispersing, allNonDispersers) # update the wolves object to remove the dead dispersers (used later to decide the empty patches)

      if(params(sim)$wolfAlps$run.tests){
        expect_equivalent(NLcount(stillDispersing), numStillDispersing - sum(dieDispersers))
      }
    }
    sim$out_numDeadDisp <- c(sim$out_numDeadDisp, numStillDispersing - NLcount(stillDispersing))

  } # end of if(NLcount(dispersers) =! 0)

  return(invisible(sim))
}

### Events (poisoning, mortality affecting a whole pack)
wolfAlpsPoisoning <- function(sim) {
  return(invisible(sim))
}

### Events (shooting, mortality affecting wolves individually)
wolfAlpsShooting <- function(sim) {
  return(invisible(sim))
}

### Events (immigration of a new wolf into the population)
wolfAlpsImmigration <- function(sim) {

  # The new wolf is created at some location and it is included in the same sim$wolves object
  # i.e., same population and will undergo the same events as the other wolves
  sim$wolves <- sprout(n = 1,
                       patches = params(sim)$wolfAlps$locImmigrant, # location of the immigrant wolf
                       breed = "wolf", turtles = sim$wolves)
  # Update immigrant information (need to add genetic information)
  sim$wolves <- NLset(turtles = sim$wolves,
                      agents = turtle(sim$wolves, who = max(of(agents = sim$wolves, var = "who"))),
                      var = c("CMR", "sex", "ageClass", "alpha", "dispersing", "age"),
                      val = cbind.data.frame(CMR = 0, sex = "F", ageClass = "juvenile", alpha = 0,
                                             dispersing = 1, age = 2))

  return(invisible(sim))
}
