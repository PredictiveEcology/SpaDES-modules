## Module metadata
defineModule(sim, list(
  name = "caribou2Movements",
  description = "Simulate a two-behavior movement for caribou",
  keywords = c("caribou", "individual based movement model", "random walk",
               "biased correlated random walk", "foray loop", "seasonal fidelity"),
  authors = c(person("Sarah", "Bauduin",
                     email = "sarah.bauduin.1@ulaval.ca", role = c("aut", "cre")),
              person(c("Eliot", "J", "B"), "McIntire",
                     email = "Eliot.McIntire@NRCan.gc.ca", role = c("cre"))),
  childModules = character(),
  version = numeric_version("1.0.3"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "day",
  citation = list(),
  documentation = list("README.txt", "caribou2Movements.Rmd"),
  reqdPkgs = list("sp", "maptools", "raster", "CircStats", "data.table", "rgeos"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", 0, NA, NA, "time to schedule first plot event"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "time interval between plot events"),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA, "time to schedule first save event"),
    defineParameter(".saveInterval", "numeric", 1, NA, NA, "time interval between save events"),
    defineParameter("N_subpop", "numeric", NA_real_, NA, NA, "vector of number of individuals in each subpopulation"),
    defineParameter("dates_MA", "numeric", NA_real_, NA, NA, "vector with the julian dates for the beginning and the end of the mating season"),
    defineParameter("dates_season", "numeric", NA_real_, NA, NA, "vector with the julian dates for the seasons which change the movement behavior"),
    defineParameter("scale_world", "numeric", NA_real_, NA, NA, "size of a raster cell side in meters"),
    defineParameter("mean_step_good", "numeric", NA_real_, NA, NA, "mean step length in good quality habitat in log(m)"),
    defineParameter("mean_step_bad", "numeric", NA_real_, NA, NA, "mean step length in low quality habitat in log(m)"),
    defineParameter("sd_step", "numeric", NA_real_, NA, NA, "step length standard deviation in log(m)"),
    defineParameter("max_step", "numeric", NA_real_, NA, NA, "maximum step length in meters"),
    defineParameter("pCross_good", "numeric", NA_real_, NA, NA, "probability to cross a road when in good quality habitat"),
    defineParameter("pCross_bad", "numeric", NA_real_, NA, NA, "probability to cross a road when in low quality habitat"),
    defineParameter("sd_matingAttract", "numeric", NA_real_, NA, NA, "sd of the wrapped normal distribution for the mating area attraction"),
    defineParameter("move_model2", "character", NA_real_, NA, NA, "movement behavior in low quality habitat ('BCRW' or 'FL')"),
    defineParameter("sd_corr", "numeric", NA_real_, NA, NA, "sd of the wrapped normal distribution for the correlation movement"),
    defineParameter("sd_bias", "numeric", NA_real_, NA, NA, "sd of the wrapped normal distribution for the bias movement"),
    defineParameter("max_bias", "numeric", NA_real_, NA, NA, "maximum distance from a good quality cell for the bias to occur (in km)"),
    defineParameter("sd_fl", "numeric", NA_real_, NA, NA, "sd of the wrapped normal distribution for the foray loop movement"),
    defineParameter("maxStep_loop", "numeric", NA_real_, NA, NA, "maximum number of steps in a loop for the outgoing movement")),
  inputObjects = data.frame(
    objectName = c("unique_MA", ##list of SpatialPolygons. Each item is a mating area for a subpopulation, ordered in the same order as N_subpop
                   "rsf_summer", ##raster habitat quality (summer)
                   "rsf_winter", ##raster habitat quality (winter)
                   "distance_good_summer", ##raster distance to the closest cell of good quality (summer)
                   "distance_good_winter", ##raster distance to the closest cell of good quality (winter)
                   "direction_good_summer", ##raster direction to the closest cell of good quality (summer)
                   "direction_good_winter", ##raster direction to the closest cell of good quality (winter)
                   "paved_roads"), ##raster paved roads barriers
    objectClass = c("list", "raster", "raster", "raster", "raster", "raster", "raster", "raster"),
    sourceURL = c("https://www.dropbox.com/s/iraohbky8eifaov/inputs_caribou2Movements.RData",
                  "", "", "", "", "", "", ""),
    other = rep(NA_character_, 8),
    stringsAsFactors = FALSE),
  outputObjects = data.frame(
    objectName = character(), objectClass = character(),
    other = character(), stringsAsFactors = FALSE)
))

## short url for data:
# https://www.dropbox.com/s/iraohbky8eifaov/inputs_caribou2Movements.RData?dl=1
# https://goo.gl/GlUfq7

## Event types
doEvent.caribou2Movements = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType == "init") {

    sim <- sim$caribou2MovementsInit(sim)

    sim <- scheduleEvent(sim,params(sim)$caribou2Movements$.plotInitialTime,"caribou2Movements","plotInit")
    sim <- scheduleEvent(sim,params(sim)$caribou2Movements$.saveInitialTime,"caribou2Movements","save")
    sim <- scheduleEvent(sim,start(sim),"caribou2Movements","caribouMove")
    sim <- scheduleEvent(sim,end(sim)-0.01,"caribou2Movements","endEvent") ##just before the end, create the outputs

  } else if (eventType == "plotInit") {
    # Plot the landscape empty
    land <- sim$worldQuality; land[] <- 0
    #Plot(land,cols="white",legend=FALSE,title=FALSE)
    Plot(rsf_summer)

    # Plot the caribou locations
    sim <- sim$caribou2MovementsPlot(sim)
    sim <- scheduleEvent(sim,time(sim)+params(sim)$caribou2Movements$.plotInterval,"caribou2Movements","plot")

  } else if (eventType == "plot") {

    # Plot the caribou locations
    sim <- sim$caribou2MovementsPlot(sim)
    sim <- scheduleEvent(sim,time(sim)+params(sim)$caribou2Movements$.plotInterval,"caribou2Movements","plot")

  } else if (eventType == "save") {

    # Record the caribou locations
    sim <- sim$caribou2MovementsSaveLoc(sim)
    sim <- scheduleEvent(sim,time(sim)+params(sim)$caribou2Movements$.saveInterval,"caribou2Movements","save")

  } else if (eventType == "caribouMove") {

    # Caribou movement
    sim <- sim$caribou2MovementsMove(sim)
    sim <- scheduleEvent(sim,time(sim)+1,"caribou2Movements","caribouMove")

  } else if (eventType == "endEvent") {

    # Record the last locations and create the raster output
    sim <- sim$caribou2MovementsSaveLoc(sim)
    sim <- sim$caribou2MovementsSaveRaster(sim)

  } else {
    warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                   "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}


## Event functions

### Initilization
caribou2MovementsInit <- function(sim) {

  sim$visitedCells <- numeric() ##vector to store the visited cells

  ## Model starts in winter
  sim$worldQuality <- sim$rsf_winter
  sim$world_rasterStack <- stack(sim$worldQuality,sim$paved_roads)
  sim$world_rasterStack@layers[[1]]@data@names <- "layer.1"
  sim$world_rasterStack@layers[[2]]@data@names <- "layer.2"
  sim$distance_good <- sim$distance_good_winter
  sim$direction_good <- sim$direction_good_winter

  ## Caribou variables
  ##Population composition
  caribouName <- c()
  for(i in 1:sum(params(sim)$caribou2Movements$N_subpop)){
    caribouName[i] <- paste("car",i,sep="")
  }
  seq_ind <- seq_len(length(caribouName))

  ## Mating areas
  ## Assign each unique_MA to all individuals regarding their subpopulation
  #sim$matingAreas <- list()
  #Eliot5 - user a single crs for the mating area maps... they weren't matching correctly
  sim$crsMatingArea <- crs(sim$unique_MA[[1]])

  #Eliot4 - don't make individual matingAreas, just use an index, i.e., individual 13 uses unique_MA 1
  #Eliot4 k <- 1
  #Eliot4  for(i in 1:length(params(sim)$caribou2Movements$N_subpop)){
  #Eliot4    for(j in 1:params(sim)$caribou2Movements$N_subpop[i]){
  #Eliot4      sim$matingAreas[k] <- sim$unique_MA[[i]]
  #Eliot4      k <- k+1
  #Eliot4    }
  #Eliot4  }
  sim$matingAreaDT <- data.table(
    caribouName = as.character(caribouName),   #Eliot4
    caribouNum = 1:length(caribouName),  #Eliot4
    matingArea = rep(1:length(params(sim)$caribou2Movements$N_subpop),  #Eliot4
                     times = params(sim)$caribou2Movements$N_subpop))  #Eliot4

  ## Assign the individuals a random locations inside their mating area
  matingAreas_loc_list <- list()
  for(i in 1:length(sim$unique_MA)){
    matingAreas_loc_list[[i]] <- dotsInPolys(sim$unique_MA[[i]],as.integer(params(sim)$caribou2Movements$N_subpop[i]),f="random",compatible=TRUE)[[1]]
  }
  matingAreas_loc <- do.call(rbind,matingAreas_loc_list)
  caribouPosition <- cbind(matingAreas_loc[,1],matingAreas_loc[,2]) ##caribou initial positions in their mating areas (need to build the matrix like this for the column names)
  inMatingAreas <- rep.int(1,length(caribouName)) ##all individuals are inside their mating area

  caribou_distGood <- extract(sim$distance_good,caribouPosition) ##caribou distance for the closest cell of good quality
  caribouHeading <- runif(length(caribouName),min=0,max=360) ##heading at random at the beginning
  caribou_FLaway <- rep.int(TRUE,length(caribouName)) ##when caribou will start their foray loop, first they will move away
  caribou_NSteps <- rep.int(0,length(caribouName)) ## caribou have not done any step in a loop yet
  loop_start <- caribouPosition ##for the first time step, their initial positions are the starting points for the loop

  ## create the caribou agent object
  sim$caribou <- SpatialPointsDataFrame(coords=caribouPosition, data=data.frame(caribouPosition[,1],caribouPosition[,2],
                                                                              caribouName,seq_ind,
                                                                              matingAreas_loc[,1],matingAreas_loc[,2],
                                                                              inMatingAreas,caribou_distGood,caribouHeading,
                                                                              caribou_FLaway,caribou_NSteps,
                                                                              loop_start[,1],loop_start[,2]))

  ## Create the objects to store the caribou locations (df) and visits (raster)
  sim$caribou_loc <- data.frame(year=numeric(),day=numeric(),caribouPosition...1.=numeric(),caribouPosition...2.=numeric())
  sim$raster_visits <- sim$worldQuality ##raster output same as input
  sim$raster_visits[] <- 0 ##reset the value to 0

  return(invisible(sim))
}


### Save/outputs events
caribou2MovementsSaveLoc <- function(sim) {

  ## Append the new locations to the previous ones
  sim$caribou_loc <- rbind(sim$caribou_loc,data.frame(year=(floor(rep(time(sim,"year"),length(sim$caribou))))+1,day=rep(ceiling(time(sim)%%(dyear(1)/dday(1))),length(sim$caribou)),sim$caribou@data[,1:2]))
  return(invisible(sim))
}

caribou2MovementsSaveRaster <- function(sim) {

  ## Transfer the number of visit per cells on the raster
  visit_values <- as.vector(table(sim$visitedCells))
  visit_cell_ids <- unique(sim$visitedCells)[order(unique(sim$visitedCells))]
  sim$raster_visits[visit_cell_ids] <- visit_values ##number of caribou visits on the cells

  return(invisible(sim))
}


### Plot event
caribou2MovementsPlot <- function(sim) {
  Plot(sim$caribou,addTo="rsf_summer",pch=".") ##plot the positions of the caribou on the empty landscape
  return(invisible(sim))
}


### Model event, caribou movement, one time step
caribou2MovementsMove <- function(sim) {

  ## Date
  julian_day <- ceiling(time(sim)%%(dyear(1)/dday(1))) ##obtain julian date

  ## Update the data regarding the season
  if (julian_day == params(sim)$caribou2Movements$dates_season[1]){ ##beginning of "summer" for the RSF (May 1st)
    sim$worldQuality <- sim$rsf_summer
    sim$world_rasterStack <- stack(sim$worldQuality,sim$paved_roads)
    sim$world_rasterStack@layers[[1]]@data@names <- "layer.1"
    sim$world_rasterStack@layers[[2]]@data@names <- "layer.2"
    sim$distance_good <- sim$distance_good_summer
    sim$direction_good <- sim$direction_good_summer
  }
  if (julian_day == params(sim)$caribou2Movements$dates_season[2]){ ##beginning of "winter" for the RSF (November 16th)
    sim$worldQuality <- sim$rsf_winter
    sim$world_rasterStack <- stack(sim$worldQuality,sim$paved_roads)
    sim$world_rasterStack@layers[[1]]@data@names <- "layer.1"
    sim$world_rasterStack@layers[[2]]@data@names <- "layer.2"
    sim$distance_good <- sim$distance_good_winter
    sim$direction_good <- sim$direction_good_winter
  }

  ## Deconstruct the SpatialPointsDataFrame to run the functions on vectors instead (faster)
  caribouPosition <- cbind(sim$caribou@data$caribouPosition...1.,sim$caribou@data$caribouPosition...2.)
  caribouName <- sim$caribou@data$caribouName
  seq_ind <- sim$caribou@data$seq_ind
  matingAreas_loc <- cbind(x=sim$caribou@data$matingAreas_loc...1.,y=sim$caribou@data$matingAreas_loc...2.)
  inMatingAreas <- sim$caribou@data$inMatingAreas
  caribou_distGood <- sim$caribou@data$caribou_distGood
  caribouHeading <- sim$caribou@data$caribouHeading
  caribou_FLaway <- sim$caribou@data$caribou_FLaway
  caribou_NSteps <- sim$caribou@data$caribou_NSteps
  loop_start <- cbind(sim$caribou@data$loop_start...1.,sim$caribou@data$loop_start...2.)

  ## Define a step length for the next move of the caribou from a log normal distribution with a maximum value threshold
  ## Different step lentgh means depending on the habitat quality
  stepsGood <- stepLengths(N_agents=length(caribouName),
                         mean_logN=params(sim)$caribou2Movements$mean_step_good,
                         sd_logN=params(sim)$caribou2Movements$sd_step,
                         max_step=params(sim)$caribou2Movements$max_step)
  stepsBad <- stepLengths(N_agents=length(caribouName),
                        mean_logN=params(sim)$caribou2Movements$mean_step_bad,
                        sd_logN=params(sim)$caribou2Movements$sd_step,
                        max_step=params(sim)$caribou2Movements$max_step)
  caribouSteps <- ifelse(caribou_distGood == 0,stepsGood,stepsBad) ##assign the caribou step lengths regarding the quality of the cell is on

  ## Define the caribou next possible locations as the unique cells around them at their caribouSteps distance
  caribouCirclesDT <- cellsCircle(positions=caribouPosition,
                              buffers=caribouSteps,
                              raster_world=sim$worldQuality,
                              scale_world=params(sim)$caribou2Movements$scale_world)

  ## Define all the pathways for the individuals
  ## One pathway = cells on the lines between the current position and one potential next location (cell on the individual circle)
  caribouPaths <- cellsLine(positions=caribouPosition,
                          next_locations=caribouCirclesDT,
                          distances=caribouSteps,
                          raster_world=sim$worldQuality,
                          scale_world=params(sim)$caribou2Movements$scale_world)

  ## Extract raster values from the rasterstack for the individual pathways
  ## Calculate their mean habitat quality value and the impact of road presence (pCross^number of roads)
  pCross <- ifelse(caribou_distGood == 0,params(sim)$caribou2Movements$pCross_good,params(sim)$caribou2Movements$pCross_bad) ##the road crossing probability for the caribou depends of the quality of the cell is on
  pathFeatures <- featureLines(pathways=caribouPaths,
                             rasterStack_world=sim$world_rasterStack,
                             pCross=pCross)

  ## Calculate the probabilities of chosing the pathways for the caribou based on their qualities
  pathQuality <- pathFeatures$mean_value_raster
  path_pQuality <- pRescale(pathValues=pathQuality) ##pathway probabilities sum to 1 for each individual

  ## Calculate the probabilities of chosing the pathways for the caribou based on the road presence
  pathRoad <- pathFeatures$cross_value_raster
  path_pRoad <- pRescale(pathValues=pathRoad) ##pathway probabilities sum to 1 for each individual
  for(i in 1:length(caribouName)){
    if(sum(pathRoad[[i]]) == length(pathRoad[[i]])){ ##when there is no road around one caribou
      path_pRoad[[i]] <- rep.int(1,length(pathRoad[[i]])) ##set probabilities equal to 1 to not influence the final combined pathway probabilities
    }
  }

  ## For the rest of the computation, it is faster to use lists for caribouCircles and caribouPaths_list instead of data.table
  caribouCircles_df <- as.data.frame(caribouCirclesDT)
  caribouCircles <- split(caribouCircles_df[,c("x","y")],caribouCircles_df$ids) ##split by individuals
  #Eliot3 - don't convert to list - keep as data.table - the "split" function is slow-ish
  #Eliot3  caribouPaths_df <- as.data.frame(caribouPaths)
  #Eliot3  caribouPaths_list <- split(caribouPaths_df,caribouPaths_df$ids) ##split by individuals

  setkey(caribouPaths, ids)#Eliot3
  #Eliot3   caribouPaths <- lapply(seq_ind, function(ind_car)
  #Eliot3   split(caribouPaths_list[[ind_car]][,3],caribouPaths_list[[ind_car]][,2])) ##split the individual by pathways

  ## Attraction of the individuals towards their mating area during mating season
  ## Mating season defined from September 15th to November 1st
  mating_period <- ifelse(julian_day >= params(sim)$caribou2Movements$dates_MA[1] && julian_day <= params(sim)$caribou2Movements$dates_MA[2], ##define if it is mating season or not
                        TRUE,
                        FALSE)
  ## Is the individual already inside its mating area
  if(mating_period == TRUE){
    # Eliot1 use gIntersects, with byid=TRUE which is faster than gIntersection in a loop
    # Eliot1 for(i in 1:length(caribouName)){
    # Eliot1     inMatingAreas[i] <- ifelse(length(gIntersection(SpatialPoints(matrix(caribouPosition[i,],ncol=2),
    # Eliot1                                                                 proj4string=crsMatingArea),
    # Eliot1                                                 sim$matingAreas[[i]])) == 1,
    # Eliot1                              1,0) ##if the individual location is inside its mating area give 1, otherwise 0
    # Eliot1  }
    for(j in 1:length(params(sim)$caribou2Movement$N_subpop)) # Eliot1
      inMatingAreas[sim$matingAreaDT[matingArea == j,caribouNum]] <-# Eliot1
        as.numeric(gIntersects(SpatialPoints(matrix(caribouPosition[sim$matingAreaDT[matingArea == j,caribouNum],], ncol=2),# Eliot1
                  proj4string=sim$crsMatingArea), sim$unique_MA[[j]], byid=TRUE))# Eliot1
  }
  ## Calculate the probabilities of chosing the pathways for the caribou based on the deviation with the direction towards their matingAreas
  path_pMating <- ifelse(rep.int(mating_period,length(caribouName)) & inMatingAreas == 0, ##if it is mating season and the individual is not inside its mating area
                       ## Calculate the pathway probabilities based on rotation angle with the direction towards the caribou mating area using a wrapped normal distribution. Probabilities are rescaled to sum to 1
                       pRescale(pathValues=angle2dir(origin=caribouPosition,
                                                     oneDir=matingAreas_loc,
                                                     secondDir=caribouCircles,
                                                     mean_wrapN=0,
                                                     sd_wrapN=params(sim)$caribou2Movements$sd_matingAttract)),
                       pPath_1(N_paths=caribouCircles)) ##if it is not mating season or the individual is already inside its mating area, set probabilities equal to 1 to not influence the final combined pathway probabilities

  ## Which movement model each caribou will follow based on the quality of the cell is on
  ## Good quality -> random walk or Low quality -> complex walk
  move_model <- ifelse(caribou_distGood == 0,
                     "random_walk", ##the caribou follows a random walk
                     "complex_walk") ##otherwise it follows a more complex walk: biased correlated random walk or foray loop based on the choice by the observer

  ## Biased correlated random walk
  if(params(sim)$caribou2Movements$move_model2 == "BCRW"){

    ## Correlation part
    ## Calculate the probability of chosing the pathways for the caribou based on the rotation with the previous direction (heading)
    pathCorr <- angleHead(origin=caribouPosition,
                        heading=caribouHeading,
                        secondDir=caribouCircles,
                        mean_wrapN=0,
                        sd_wrapN=params(sim)$caribou2Movements$sd_corr)
    path_pCorr <- pRescale(pathValues=pathCorr) ##pathway probabilities sum to 1 for each individual

    ## Bias part
    ## Calculate the probability of chosing the pathways for the caribou based on the bias towards the closest good area
    caribou_dirGood <- extract(sim$direction_good,caribouPosition) ##extract the direction towards the closest good area
    pathBias <- angleHead(origin=caribouPosition,
                        heading=caribou_dirGood,
                        secondDir=caribouCircles,
                        mean_wrapN=0,
                        sd_wrapN=params(sim)$caribou2Movements$sd_bias)
    path_pBias <- pRescale(pathValues=pathBias) ##pathway probabilities sum to 1 for each individual
    no_bias <- which(caribou_distGood>params(sim)$caribou2Movements$max_bias) ##if a caribou is too far from a good area, there is no bias towards it
    for(pos_car in no_bias){
      path_pBias[[pos_car]] <- rep.int(1,length(path_pBias[[pos_car]])) ##set probabilities equal to 1 to not influence the final combined pathway probabilities
    }

    ## Calculate the pathway final probabilies including the probabilities based on the characteristics applying to the movement model
    #Eliot3 caribou_pPath <- lapply(seq_ind,function(i) ifelse(rep.int(move_model[[i]] == "random_walk",length(caribouPaths[[i]])),
    #Eliot3                                                    path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]], ##movement characteristics for the RW model
    #Eliot3                                                    path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]]*path_pCorr[[i]]*path_pBias[[i]])) ##movement characteristics for the BCRW model
    len <- caribouPaths[j=list(len=length(unique(ids_path))),by=ids][,len]    #Eliot3
    caribou_pPath <- lapply(seq_ind,function(i) ifelse(rep.int(move_model[[i]] == "random_walk",len[i]), #Eliot3
                                                       path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]], ##Eliot3 ##movement characteristics for the RW model
                                                       path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]]*path_pCorr[[i]]*path_pBias[[i]])) #Eliot3##movement characteristics for the BCRW model

  } ##end of the part on the BCRW

  ## Foray loop
  if(params(sim)$caribou2Movements$move_model2 == "FL"){

    ## Calculate the probability of chosing the pathways for the caribou based on a looping movement
    ## Repulsion (if the caribou is leaving) and attraction (if the caribou is going back) of the good habitat last visited
    pathForay <- forayLoop(going_away=caribou_FLaway,
                         num_steps_done=caribou_NSteps,
                         num_steps_max=params(sim)$caribou2Movements$maxStep_loop,
                         origin=caribouPosition,
                         loop_starting_point=loop_start,
                         secondDir=caribouCircles,
                         mean_wrapN=0,
                         sd_wrapN=params(sim)$caribou2Movements$sd_fl)
    path_pForay <- pRescale(pathValues=pathForay$prob_pathFL) ##pathway probabilities sum to 1 for each individual

    caribou_FLaway <- pathForay$going_away ##update the movement done by the caribou (going away or back)
    caribou_NSteps <- caribou_NSteps+1 ##increase the number of steps

    ## Calculate the pathway final probabilies including the probabilities based on the characteristics applying to the movement model
    #Eliot3     caribou_pPath <- lapply(seq_ind,function(i) ifelse(rep.int(move_model[[i]] == "random_walk",length(caribouPaths[[i]])),
    #Eliot3                                                      path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]], ##movement characteristics for the RW model
    #Eliot3                                                      path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]]*path_pForay[[i]])) ##movement characteristics for the FL model
    len <- caribouPaths[j=list(len=length(unique(ids_path))),by=ids][,len]    #Eliot3
    caribou_pPath <- lapply(seq_ind,function(i) ifelse(rep.int(move_model[[i]] == "random_walk",len[i]),#Eliot3
                                                path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]], #Eliot3##movement characteristics for the RW model
                                                path_pQuality[[i]]*path_pRoad[[i]]*path_pMating[[i]]*path_pForay[[i]])) #Eliot3##movement characteristics for the FL model

  } ##end of the part on the FL

  ## Movement
  pathSelected <- selectPath(pPath=caribou_pPath) ##select one pathway per individual based on their final probabilities
  # Eliot6 - do via data.table
  pathSelectedV <- data.table(ids=1:length(pathSelected),#Eliot6
                              ids_path=unlist(pathSelected),#Eliot6
                              key="ids,ids_path")#Eliot6
  setkey(caribouCirclesDT, "ids", "ids_path")#Eliot6
  caribouDestin <- as.matrix(caribouCirclesDT[pathSelectedV, list(x,y)]) #Eliot6
  #Eliot6 caribouDestin <- as.matrix(rbindlist(lapply(seq_ind,function(i) caribouCircles[[i]][pathSelected[[i]],]))) ##select ending cell of the selected pathway

  caribouHeading <- deg(atan2(x=caribouDestin[,1]-caribouPosition[,1],y=caribouDestin[,2]-caribouPosition[,2])) ##update the caribou heading towards the destination cell
  caribouHeading[caribouHeading<0] <- caribouHeading[caribouHeading<0]+360 ##atan2() gives results between -180? and 180?, so they need to be re-adjusted between 0 and 360?
  caribouPosition <- move(origin=caribouPosition, ##update the individual new positions with the pathway heading and step length
                        heading=caribouHeading,
                        length=caribouSteps)

  ## Update caribou features
  caribou_distGood <- extract(sim$distance_good,caribouPosition) ##update the distance to the closest good cell
  caribou_distGood[is.na(caribou_distGood)] <- 9999999 ##if the caribou is outside the study area, the raster value would be NA. Replace it with 9999999
  if(params(sim)$caribou2Movements$move_model2 == "FL"){
    loop_start[caribou_distGood == 0,] <- caribouPosition[caribou_distGood == 0,] ##if the new location is a good cell, it stands  as the starting of the next loop
    caribou_FLaway[caribou_distGood == 0 & move_model == "complex_walk" & caribou_FLaway == FALSE] <- TRUE ##and for the next move the caribou will go away from this location
    caribou_NSteps[caribou_distGood == 0] <- 0 ##and the number of steps for the next new loop is reset to 0
  }

  ## Reconstruct the SpatialPointsDataFrame
  sim$caribou <- SpatialPointsDataFrame(coords=caribouPosition, data=data.frame(caribouPosition[,1],caribouPosition[,2],
                                                                              caribouName,seq_ind,
                                                                              matingAreas_loc[,1],matingAreas_loc[,2],
                                                                              inMatingAreas,caribou_distGood,caribouHeading,
                                                                              caribou_FLaway,caribou_NSteps,
                                                                              loop_start[,1],loop_start[,2]))

  ## Outputs
  ## Raster of visited cell
  if((floor(time(sim,"year")))+1>=2){ ##update the cell's number of visit starting the second year to remove the biased from initial locations
    # Eliot6 - do via data.table
    pathSelectedV <- data.table(ids=1:length(pathSelected),#Eliot6
                                ids_path=unlist(pathSelected),#Eliot6
                                key="ids,ids_path")#Eliot6
    setkey(caribouPaths, "ids", "ids_path")#Eliot6
    pathCells <- caribouPaths[pathSelectedV, pixels_path]#Eliot6
    #Eliot6 pathCells <- unlist(lapply(seq_ind, function(i) caribouPaths[[i]][[pathSelected[[i]]]])) ##extract the cell IDs for the pathways selected

    sim$visitedCells <- c(sim$visitedCells,pathCells)
  }

  return(invisible(sim))
}


## Functions used in the main event (caribou movement)

stepLengths <- function(N_agents,mean_logN,sd_logN,max_step){ ##define step lentghs for N_agents from a log normal distribution with a maximum threshold (max_step in meters)
  steps_simulated <- exp(rnorm(n=N_agents,mean=mean_logN,sd=sd_logN)) ##simulate N_agents values from the log normal distribution
  for(a in 1:N_agents){
    while(steps_simulated[a]>max_step){ ##as long as one value is above the threshold given
      steps_simulated[a] <- exp(rnorm(1,mean=mean_logN,sd=sd_logN)) ##resimulate it
    }
  }
  return(steps_simulated) ##one vector of N_agents values = one step lentgh in meters for each individual
}

cellsCircle <- function(positions,buffers,raster_world,scale_world,speedup=1){ ##identify the cells at the buffer distances of the positions
  seq_num_ind <- seq_len(nrow(positions)) ##create an index sequence for the number of individuals regarding the number of positions
  n.angles <- pmin(200,ceiling({{buffers/scale_world}*16}/speedup)) ##number of equidistant points to create on the circle for a given individual to try locating every cells
  # *16 to create two points in each of the 8 neighboring cells at least
  #limit at 200 points per individual for computing time reason, speedup can reduce that limit and create even less points but proportional to the number of potential cells on the circle

  ## Eliot's code to replace the createCircle of the package PlotRegionHighlighter (faster this way)
  ids <- rep.int(seq_num_ind,times=n.angles) ##create individual IDs for the number of points that will be done for their circle
  rads <- rep.int(buffers,times=n.angles) ##create a vector of radius angle for the number of points that will be done for each individual circle
  xs <- rep.int(positions[,1],times=n.angles) ##extract and repeat the individual current position
  ys <- rep.int(positions[,2],times=n.angles)
  angle.inc <- rep.int(2*pi,length(n.angles))/n.angles ##calculate the angle increment that each individual needs to do n.angles times to complete a circle (2 pi)
  angs <- rep.int(angle.inc,times=n.angles) ##repeat this angle increment the number of times it needs to be done to complete the circles
  increments <- data.table(ids,angs) ##attach the angle increment with the individual IDs
  increments[,angles:=cumsum(angs),by=ids] ##calculate the total rotation angle for each point of the circles regarding to the direction to the first point
  # Calculate the x and y coordinates of the points on the circles
  x <- cos(increments[,angles])*rads+xs
  y <- sin(increments[,angles])*rads+ys

  coordinates_all_ind <- cbind(x,y) ##put the coordinates of the points on the circles from all individuals in the same matrix
  pixels_under_coordinates <- cellFromXY(raster_world,coordinates_all_ind) ##extract the cell IDs under the points
  pixels_ind_ids <- unique(data.table(ids,pixels_under_coordinates)) ##associate the cell IDs with the individuals and keep unique entries (remove duplicated cells for each caribou)

  unique_pixels <- unique(pixels_under_coordinates) ##extract all unique cell IDs among all individuals
  #Eliot2 - extract is slow under many conditions. Faster to take all values, and extract from matrix
  tmp  <-  raster_world[]
  unique_pixels_values <- tmp[unique_pixels]
  rm(tmp)
  #unique_pixels_values <- extract(raster_world,unique_pixels) ##extract the raster values under the unique cell IDs
  pixels_values_idsNA <- cbind(unique_pixels,unique_pixels_values) ##bind the raster values with the unique cell IDs (there may be NAs)
  pixels_unique_ids_noNA <- pixels_values_idsNA[!is.na(pixels_values_idsNA[,2]),1] ##keep the cells for which the value is not NA (i.e., inside the study area)
  coordinates_unique_pixels_noNA <- xyFromCell(raster_world,pixels_unique_ids_noNA) ##extract the coordinates for these cells
  pixels_values_ids2 <- data.table(pixels_unique_ids_noNA,coordinates_unique_pixels_noNA) ##attach the x and y coordinates with the unique cell IDs

  setkey(pixels_ind_ids,"pixels_under_coordinates") ##set a common key to merge the 2 data.tables
  setkey(pixels_values_ids2,"pixels_unique_ids_noNA")
  pixels_ind_ids_merged <- pixels_ind_ids[J(pixels_values_ids2)] ##merge the coordinates of the cells with the individual IDs
  coord_circle_ind <- subset(pixels_ind_ids_merged,select=c(ids,x,y)) ##keep only the individual IDs with the coordinates of the unique pixels on their circle
  setkey(coord_circle_ind,"ids") ##sort the data.table by individual IDs

  lack <- seq_num_ind[!seq_num_ind %in% unique(coord_circle_ind[,ids])] ##check if one individual doesn't have any cell on its circle (if step lentgh too small)
  if(length(lack!=0)){
    coord_circle_ind <- rbindlist(list(coord_circle_ind,cbind.data.frame(ids=lack,x=positions[lack,1],y=positions[lack,2]))) ##put its current location as the cell on its circle
    setkey(coord_circle_ind,"ids") ##re-sort the data.table by individual IDs
  }

  return(coord_circle_ind) ##a data.table with x and y coordinates of all unique cells on the circle around each individual
}

cellsLine <- function(positions,next_locations,distances,raster_world,scale_world,speedup=1){ ##extract the cells on the lines from one position and each of its next_locations
  seq_num_ind <- seq_len(nrow(positions)) ##create an index sequence for the number of individuals
  points_needed <- ceiling({distances/scale_world}/speedup) ##number of points needed to sample on each line to locate all cells (=number of cells at the scale_world)
  init_pos <- data.table(positions,points_needed,seq_num_ind,key="seq_num_ind") ##key=individual IDs

  #Eliot8 these next two lines are a one liner with data.table, but still need num_lines later
  num_lines <- next_locations[,list(V1=.N), by=ids][,V1] ##number of lines for each individual (=number of ending points x coordinates)
  #Eliot8 next_locations[,ids_path:=unlist(lapply(num_lines, seq_len))] ##ending line point position with path IDs for each individual
  next_locations[,ids_path:=seq_len(.N), by=ids] #Eliot8 ##number of lines for each individual (=number of ending points x coordinates)
  pos_next <- next_locations[J(init_pos)] ##join the initial positions with their ending points
  pos_next[,heading:=atan2(x=x-V1,y=y-V2)] ##angle of the line from the individual position to one ending point with 0?
  pos_next[,dist:=sqrt({x-V1}*{x-V1}+{y-V2}*{y-V2})] ##actual distance between the individual position and the ending points of their lines (because it can be slightly different than the given distances)
  pos_next[,increments_by_one:=dist/points_needed] ##distance between each point that will be created on the lines
  pos_next[,multiple:=as.numeric(points_needed)] ##how many points on the line
  setkey(pos_next,"multiple")

  pos_next_duplicated <- pos_next[J(rep(unique(multiple),unique(multiple))),allow.cartesian=TRUE] ##duplicate the rows as many times as the number of points needed for each line
  setkeyv(pos_next_duplicated,c("ids","ids_path")) ##order the data by individual and path IDs
  incr_time <- rep.int(lapply(points_needed, function(a) seq_len(a)),as.numeric(num_lines))
  pos_next_duplicated[,increments_time:=unlist(incr_time)] ##increment number for the points created on the line to know their distances from the initial position
  pos_next_duplicated[,increments:=increments_by_one*increments_time] ##distance of each points from the individual position

  pos_next_duplicated[,new_x:=cos(heading)*increments+V1] ##calculate the coordinates of the points created on the lines
  pos_next_duplicated[,new_y:=sin(heading)*increments+V2]
  pos_next_duplicated[,pixels_path:=cellFromXY(raster_world,matrix(c(new_x,new_y),ncol=2))] ##extract the cell IDs under the coordinates of the points on the individual lines
  pixels_per_line_per_ind <- subset(pos_next_duplicated,select=c(ids,ids_path,pixels_path)) ##keep only the cell IDs with the individual and path IDs
  ## We do not remove duplicates. Duplicated cells weight more in the path because they were sampled multiple times with the same equidistance between sampled points

  return(pixels_per_line_per_ind) ##a data.table of cell IDs for each line (individual pathways) with individual and path IDs
}

featureLines <- function(pathways,rasterStack_world,pCross){ ##calculate the pathway mean quality value and the probability of taking it using the road presence and the probability of crossing a road
  tmp1 <- rasterStack_world$layer.1[];#Eliot2
  tmp2 <- rasterStack_world$layer.2[];#Eliot2
  pathways[,`:=`(layer.1=tmp1[pixels_path],layer.2=tmp2[pixels_path])]

  #Eliot7 - just take pathways directly, no need to make a separate one then join
  #Eliot7 all_unique_pixels <- unique(pathways[,pixels_path]) ##retrieve all the cells composing all the lines/pathways and keep only the unique IDs
  #Eliot2 change - raster Extract is slow - just take whole vectors, and subset those
  #Eliot2 values_rasters <- extract(rasterStack_world,all_unique_pixels,layer=1,nl=2) ##extract the values from the rasters
  #Eliot7 values_rasters <- cbind(layer.1=tmp1[all_unique_pixels],layer.2=tmp2[all_unique_pixels])#Eliot2

  #values_rasters <- rasterStack_world[all_unique_pixels,] ##extract the values from the rasters
  #Eliot7 unique_pixels_and_values <- data.table(all_unique_pixels,values_rasters,key="all_unique_pixels") ##put the pixels IDs with theirs values

  #Eliot7 setkey(pathways,"pixels_path") ##set the key of the pathways table as the cells IDs to merge with the raster values
  #Eliot7 pixels_and_values <- pathways[J(unique_pixels_and_values)] ##join the raster values with all the pathway pixels

  #Eliot7 pixels_and_values[,mean_layer1:=mean(layer.1),by="ids,ids_path"] ##calculate the mean value for layer1 (habitat quality) per individual per pathway
  #Eliot7 pixels_and_values[,sum_layer2:=sum(layer.2),by="ids,ids_path"] ##calculate the sum of values for layer2 (road presence) per individual per pathway
  #Eliot7 val_per_path <- unique(subset(pixels_and_values,select=c(ids,ids_path,mean_layer1,sum_layer2))) ##select one instance of the pathways for each individual
  pathways[,mean_layer1:=mean(layer.1),by="ids,ids_path"] ##calculate the mean value for layer1 (habitat quality) per individual per pathway
  pathways[,sum_layer2:=sum(layer.2),by="ids,ids_path"] ##calculate the sum of values for layer2 (road presence) per individual per pathway
  val_per_path <- unique(subset(pathways,select=c(ids,ids_path,mean_layer1,sum_layer2))) ##select one instance of the pathways for each individual

  setkeyv(val_per_path,c("ids","ids_path")) ##order the data by individual and path IDs
  prob_cross_r <- data.table(ids=seq_len(length(pCross)),pCross,key="ids") ##create a data.table with the crossing probabilities for each individual
  val_per_path_r <- val_per_path[J(prob_cross_r)] ##and join it to the table with the pathway feature values
  val_per_path_r[,prob_cross:=pCross^sum_layer2] ##calculate the crossing probabilities of the pathway

  mean_split_ind <- split(val_per_path_r$mean_layer1,val_per_path_r$ids) ##split the pathway mean quality values per individuals
  cross_split_ind <- split(val_per_path_r$prob_cross,val_per_path_r$ids) ##split the pathway crossing proabilities per individuals

  return(list(mean_value_raster=mean_split_ind, ##list (one item per individual) of vectors (one value per pathway) of the mean raster habitat quality values
              cross_value_raster=cross_split_ind)) ##list (one item per individual) of vectors (one value per pathway) of pCross^(number of roads on the pathway)
}

pRescale <- function(pathValues){ ##transform the pathway feature values into probabilities so that the probabilities sum to 1 for each individual
  seq_num_ind <- seq_len(length(pathValues)) ##create an index sequence for the number of individuals
  prob_scaled <- list()
  prob_scaled[seq_num_ind] <- lapply(seq_num_ind, ##for each individual
                                   function(a) ifelse(rep.int(sum(pathValues[[a]]!=0),length(pathValues[[a]])), ##check if the mean pathValues of all the pathways is different from 0
                                                      pathValues[[a]]/sum(pathValues[[a]]), ##if yes, the pathway probability scaled is = pathway value / sum of all pathway values
                                                      rep.int(1,length(pathValues[[a]]))/length(pathValues[[a]]))) ##otherwise give an equal probability to each pathway

  return(prob_scaled) ##list of vector with the scaled probabilities (sum to 1) for each available pathway of each individual
}

angle2dir <- function(origin,oneDir,secondDir,mean_wrapN,sd_wrapN){ ##calculate the probability to rotate from the direction oneDir to the direction secondDir using a wrapped normal distribution
  num_second_direction <- unlist(lapply(secondDir,nrow)) ##retrieve the number of secondDir per individual
  x_ori <- rep.int(origin[,1],num_second_direction) ##repeat the origin and oneDir the number of times corresponding to secondDir
  y_ori <- rep.int(origin[,2],num_second_direction)
  x_first <- rep.int(oneDir[,1],num_second_direction)
  y_first <- rep.int(oneDir[,2],num_second_direction)
  ## Calculate the angle between 3 points (origin, oneDir and one of secondDir) with the following formulae
  ## theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
  ## where a and b are vectors with origin c(0,0) and are calculated as c(end(x)-origin(x),end(y)-origin(y))
  ## Values are always between 0 and 180 degrees, does not matter the direction to the right or to the left (+ or - angles)
  x_first_trans <- x_first-x_ori ##susbtract the origin point
  y_first_trans <- y_first-y_ori
  second_direction <- as.matrix(rbindlist(secondDir)) ##rbind the secondDir from all the individuals
  x_second_trans <- second_direction[,1]-x_ori
  y_second_trans <- second_direction[,2]-y_ori
  sum_a_times_b <- x_first_trans*x_second_trans+y_first_trans*y_second_trans ##decompose the formulae to apply it on vectors
  sum_a_times_a <- x_first_trans*x_first_trans+y_first_trans*y_first_trans
  sum_b_times_b <- x_second_trans*x_second_trans+y_second_trans*y_second_trans
  angles <- deg(acos(sum_a_times_b/(sqrt(sum_a_times_a)*sqrt(sum_b_times_b)))) ##calculate the angles between each 3 points (between vector origin-oneDir and vector origin-secondDir)
  angles_prob <- dnorm(angles,mean=mean_wrapN,sd=sd_wrapN) ##calculate the probability from a wrapped normal distribution (angles are alwyas between 0 and 180)
  angles_prob[is.na(angles_prob)] <- 1 ##if an angle could not be calculated (because origin=first_dir), put 1

  seq_num_ind <- seq_len(nrow(origin)) ##create an index sequence for the number of individuals
  angles_prob_split <- split(angles_prob,rep.int(seq_num_ind,num_second_direction)) ##split the results by individuals
  return(angles_prob_split) ##list (one item per individual) of vectors (one value per pathway) of proabilities to rotate from the direction towards the individual mating area
}

pPath_1 <- function(N_paths){ ##give all the pathways a probability of 1
  seq_num_ind <- seq_len(length(N_paths)) ##create an index sequence for the number of individuals
  equal_prob <- list()
  equal_prob[seq_num_ind] <- lapply(seq_num_ind, ##for each individual
                                  function(a) rep.int(1,nrow(N_paths[[a]]))) ##create a vector of 1 of length of the number of pathways

  return(equal_prob) ##list of vectors of 1 of length the number of N_paths for each individual
}

angleHead <- function(origin,heading,secondDir,mean_wrapN,sd_wrapN){ ##calculate the probability to rotate from the heading direction to the direction secondDir using a wrapped normal distribution
  angles_secondDir <- angle1dir(origin=origin,dir=secondDir) ##calculate the angle (direction) from the origin to each one of the secondDir
  num_second_points <- unlist(lapply(secondDir,nrow)) ##retrieve the number of secondDir per individual
  headings <- rep.int(heading,num_second_points) ##repeat the heading values the number of times their number of corresponding secondDirs
  angle_subtraction <- angles_secondDir-headings ##find the rotation angle between the 2 directions
  angle_subtraction[angle_subtraction>180] <- angle_subtraction[angle_subtraction>180]-360 ##angles reported are between -360? and 360?, need to be between -180 and 180
  angle_subtraction[angle_subtraction<(-180)] <- angle_subtraction[angle_subtraction<(-180)]+360

  angles_prob <- dnorm(angle_subtraction,mean=mean_wrapN,sd=sd_wrapN) ##calculate the angle probabilities from a wrapped normal distribution
  seq_num_ind <- seq_len(nrow(origin)) ##create an index sequence for the number of individuals
  angles_prob_split <- split(angles_prob,rep.int(seq_num_ind,num_second_points)) ##split the angle by individual

  return(angles_prob_split) ##list (one item per individual) of vectors (one value per pathway) of proabilities to rotate from the current heading
}

angle1dir <- function(origin,dir){ ##obtain the angles of the directions from origin to the locations in dir regarding direction north, in degrees between 0 and 360?
  num_dir <- unlist(lapply(dir,nrow)) ##retrieve the number of dir locations per individual
  x_ori <- rep.int(origin[,1],num_dir) ##repeat the origin points as many times
  y_ori <- rep.int(origin[,2],num_dir)
  dir_rbind <- as.matrix(rbindlist(dir)) ##put together all the individuals
  angle_dir <- deg(atan2(x=dir_rbind[,1]-x_ori,y=dir_rbind[,2]-y_ori)) ##calculate the angle of each direction (origine to each second_point)
  angle_dir[angle_dir<0] <- angle_dir[angle_dir<0]+360 ##atan2() gives angles between -180? and 180? so they need to be re-adjusted between 0 and 360?
  angle_dir[is.na(angle_dir)] <- 0 ##if an angle coudln't be calculated (because origin=dir), put 0

  return(angle_dir) ##vector of angle values between 0 and 360? of length dir
}

forayLoop <- function(going_away,num_steps_done,num_steps_max,origin,loop_starting_point,secondDir,mean_wrapN,sd_wrapN){ ##return the pathway probabilities for a foray loop movement
  prob_pathFL <- ifelse(going_away == TRUE & num_steps_done<num_steps_max, ##if the individual was going away from its starting point at its previous movement and it has not reach yet its maximum number of steps
                      FLaway(origin=origin,loop_starting_point=loop_starting_point,secondDir=secondDir,mean_wrapN=mean_wrapN,sd_wrapN=sd_wrapN), ##it keeps going away
                      FLback(origin=origin,loop_starting_point=loop_starting_point,secondDir=secondDir,mean_wrapN=mean_wrapN,sd_wrapN=sd_wrapN)) ##otherwise, it goes back towards its starting location

  going_away[going_away == TRUE & num_steps_done>=num_steps_max] <- FALSE ##update which move (away or back) the individual just did
  results <- list("prob_pathFL"=prob_pathFL,
                "going_away"=going_away)

  return(results) ##list with first item = list (one item per individual) of vectors (one value per pathway) of probabilities based on the foray loop movement and second item = vector of TRUE and FALSE for going away or not
}

FLaway <- function(origin,loop_starting_point,secondDir,mean_wrapN,sd_wrapN){ ## calculate the probabilities for the angles (directions) of the heading to the direction away from the loop_starting_point
  angle_to_loop_pts <- deg(atan2(x=loop_starting_point[,1]-origin[,1],y=loop_starting_point[,2]-origin[,2])) ##calculate the angle of each direction (from origin to loop_starting_point)
  angle_to_loop_pts[angle_to_loop_pts<0] <- angle_to_loop_pts[angle_to_loop_pts<0]+360 ##atan2() gives angles between -180? and 180? so they need to be re-adjusted between 0 and 360?
  angle_opposite_loop_starting_point <- angle_to_loop_pts+180 ##add 180 to these angles to find the opposite direction (going away)
  angle_opposite_loop_starting_point[angle_opposite_loop_starting_point>360] <- angle_opposite_loop_starting_point[angle_opposite_loop_starting_point>360]-360 ##rescale values greater than 360?

  ## Calculate the probabilities of the rotations of the angles just calculated (going away) with the individual current heading from a wrapped normal distribution
  angle_rotate_away <- angleHead(origin=origin,
                               heading=angle_opposite_loop_starting_point,
                               secondDir=secondDir,
                               mean_wrapN=mean_wrapN,
                               sd_wrapN=sd_wrapN)

  return(angle_rotate_away) ##list (one item per individual) of vectors (one value per pathway) of probabilities for the rotation of the pathway with the direction away from the loop start
}

FLback <- function(origin,loop_starting_point,secondDir,mean_wrapN,sd_wrapN){ ## calculate the probabilities for the angles (directions) of the heading to the direction to the loop_starting_point
  angle_to_loop_pts <- deg(atan2(x=loop_starting_point[,1]-origin[,1],y=loop_starting_point[,2]-origin[,2])) ##calculate the direction origin to loop_starting_point
  angle_to_loop_pts[angle_to_loop_pts<0] <- angle_to_loop_pts[angle_to_loop_pts<0]+360 ##atan2() gives angles between -180? and 180? so they need to be re-adjusted between 0 and 360?

  ## Calculate the probabilities of the rotation of the angles just calculated (going back) with the individual current heading from a wrapped normal distribution
  angle_rotate_back <- angleHead(origin=origin,
                               heading=angle_to_loop_pts,
                               secondDir=secondDir,
                               mean_wrapN=mean_wrapN,
                               sd_wrapN=sd_wrapN)

  return(angle_rotate_back) ##list (one item per individual) of vectors (one value per pathway) of probabilities for the rotation of the pathways with the direction towards the loop start
}

selectPath <- function(pPath){ ##select one pathway based on their probabilities
  seq_num_ind <- seq_len(length(pPath)) ##create an index sequence for the number of individuals
  pPath_sum1 <- lapply(seq_num_ind,function(a) pPath[[a]]/sum(pPath[[a]])) ##rescale the probabilities so that they sum to 1
  cum_sum_prob <- lapply(pPath_sum1,cumsum) ##calculate the cumulative sum of all the pathway probabilities
  prob <- runif(seq_num_ind,0,1) ##draw a probability value at random between 0 and 1, one for each individual
  selected_pathway_probability <- lapply(seq_num_ind,
                                       function(a) match(min(cum_sum_prob[[a]][cum_sum_prob[[a]]>prob[a]]),cum_sum_prob[[a]])) ##find the pathway position for which the drawn probability falls in the pathway probability (based on the cumulative probability sum)

  return(selected_pathway_probability) ##list (one item per individual) of unique value of the selected pathway
}

move <- function(origin,heading,length){ ##give new coordinate based on the origin location and the heading and length of the step
  origin[,1] <- origin[,1]+cos(rad(heading))*length ##convert the angle (heading) in radians
  origin[,2] <- origin[,2]+sin(rad(heading))*length
  return(origin) ##matrix with each row is the individual new position
}
