stopifnot(packageVersion("SpaDES") >= "1.0.1")

defineModule(sim, list(
  name="LccToBeaconsReclassify",
  description="Takes the LCC05 classification of 39 land cover classes, and reclassifies it to the 11 classes of the Beacons succession model.",
  keywords=c("forest succession", "LCC05", "land cover classification 2005", "Beacons"),
  childModules=character(),
  authors=c(person(c("Eliot", "J", "B"), "McIntire", email="Eliot.McIntire@NRCan.gc.ca", role=c("aut", "cre")),
            person(c("Alex", "M"), "Chubaty", email="Alexander.Chubaty@NRCan.gc.ca", role=c("aut")),
            person("Steve", "Cumming", email="Steve.Cumming@sbf.ulaval.ca", role=c("aut"))),
  version=numeric_version("0.0.3"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c("2005-01-01", NA)),
  timeunit="year",
  citation=list(),
  reqdPkgs=list("raster", "RColorBrewer", "fastmatch", "dplyr"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA_real_, NA, NA, desc="Initial time for plotting"),
    defineParameter(".plotInterval", "numeric", NA_real_, NA, NA, desc="Interval between plotting"),
    defineParameter(".saveInitialTime", "numeric", NA_real_, NA, NA, desc="Initial time for saving"),
    defineParameter(".saveInterval", "numeric", NA_real_, NA, NA, desc="Interval between save events")),
  inputObjects=data.frame(objectName="vegMapLcc",
                          objectClass="RasterLayer",
                          other=NA_character_, stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("trajMapBeacons", "vegMapBeacons", "trajObj"),
                           objectClass=c("RasterLayer", "RasterLayer", "matrix"),
                           other=rep(NA_character_, 3L), stringsAsFactors=FALSE)
))

doEvent.LccToBeaconsReclassify = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    checkObject(sim, name="vegMapLcc") # Lcc map or a clipped extent version

    sim <- sim$LccToBeaconsReclassifyInit(sim)

    sim <- scheduleEvent(sim, params(sim)$LccToBeaconsReclassify$.plotInitialTime,
                         "LccToBeaconsReclassify", "plot")
    sim <- scheduleEvent(sim, params(sim)$LccToBeaconsReclassify$.saveInitialTime,
                         "LccToBeaconsReclassify", "save")
  } else if (eventType=="plot") {
    Plot(vegMapBeacons, trajMapBeacons, new=TRUE)
    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$LccToBeaconsReclassify$.plotInterval, "LccToBeaconsReclassify", "plot")
  } else if (eventType=="save") {
    # the raster package does not keep colors when writing to a tif file

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + params(sim)$LccToBeaconsReclassify$.saveInterval, "LccToBeaconsReclassify", "save")
  } else {
      warning(paste("Undefined event type: '", events(sim)[1, "eventType", with=FALSE],
                    "' in module '", events(sim)[1, "moduleName", with=FALSE], "'", sep=""))
  }
  return(invisible(sim))
}

### template initilization
LccToBeaconsReclassifyInit = function(sim) {
  ### From the table 1 in Word file from Steve Cumming & Pierre Vernier, June 6, 2014
  ###  09 A5 MDR ANslysis V4_SL.docx
  #
  # lcc05TrajReclass <- read.table(file="clipboard", header=TRUE, sep="\t")
  # dput(lcc05TrajReclass[,c("LCC05.classes","Trajectory","Description")])
  # dput(lcc05TrajReclass[,c("LCC05.classes","VEG.reclass","Description")])
  #

  lcc05TrajReclass <- structure(
    list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
                                 .Label=c("0,30,31,32,33,36,38,39", "1",
                                          "16,35", "17,18,20,21,22,23,24,25",
                                          "19", "2,11,12", "26,27,28,29",
                                          "3,4,5,13,14,15", "34", "37",
                                          "6,7,8,9,10"), class = "factor"),
         Trajectory=structure(c(2L, 5L, 7L, 6L, 8L, 9L, 1L, 10L, 11L, 3L, 4L),
                              .Label=c("1,2,3,4,5,6", "1,3,4,5,6", "10",
                                       "11", "2,4", "3,4,5", "3,4,6", "6",
                                       "6", "8", "9"), class = "factor"),
         Description=structure(c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
                               .Label=c("Burned", "Closed coniferous", "Cropland",
                                        "Deciduous", "Herbaceous", "Mixedwood",
                                        "Open coniferous", "Other", "Shrub",
                                        "Water", "Wetland"), class = "factor")),
    .Names=c("LCC05.classes", "Trajectory", "Description"),
    class="data.frame", row.names = c(NA, -11L))


  lcc05VegReclass <- structure(
    list(LCC05.classes=structure(c(2L, 11L, 8L, 6L, 3L, 4L, 9L, 5L, 10L, 7L, 1L),
                                 .Label=c("0,30,31,32,33,36,38,39", "1",
                                          "16,35", "17,18,20,21,22,23,24,25",
                                          "19", "2,11,12", "26,27,28,29",
                                          "3,4,5,13,14,15", "34", "37",
                                          "6,7,8,9,10"), class = "factor"),
         VEG.reclass=1:11, Description=structure(
           c(2L, 7L, 6L, 4L, 9L, 5L, 1L, 11L, 10L, 3L, 8L),
           .Label = c("Burned", "Closed coniferous",  "Cropland", "Deciduous",
                      "Herbaceous", "Mixedwood", "Open coniferous", "Other",
                      "Shrub", "Water", "Wetland"), class = "factor")),
    .Names = c("LCC05.classes", "VEG.reclass", "Description"),
    class = "data.frame", row.names = c(NA, -11L))

  lcc05VegLabels <- as.numeric(strsplit(paste(lcc05VegReclass$LCC05.classes, collapse=","),",")[[1]])
  numLccInVeg <- sapply(strsplit(unname(sapply(as.character(lcc05VegReclass$LCC05.classes), function(x) x)), ","), length)
  lcc05VegTable <- cbind(lcc05VegLabels,rep(lcc05VegReclass$VEG.reclass,numLccInVeg))

  # Trajectory
  lcc05TrajLabels <- as.numeric(strsplit(paste(lcc05TrajReclass$LCC05.classes, collapse=","), ",")[[1]])
  numLccInTraj <- sapply(strsplit(unname(sapply(as.character(lcc05TrajReclass$LCC05.classes), function(x) x)), ","), length)

  lcc05TrajReclass$TrajectoryNum <- lapply(as.character(
    lcc05TrajReclass$Trajectory), function(x) as.numeric(strsplit(x,",")[[1]]))

  resample <- function(x, ...) x[sample.int(length(x), ...)]
  lcc05TrajTable <- cbind(
    lcc05TrajLabels,
    unlist(
      lapply(1:length(lcc05TrajReclass$TrajectoryNum),
             function(x)
               resample(lcc05TrajReclass$TrajectoryNum[[x]],
                      numLccInTraj[x],replace=T))))

  trajObj.raw <- structure(
    list(Veg.Type=c("Closed coniferous", "Open coniferous", "Mixedwood",
                    "Deciduous*", "Deciduous*", "Shrub", "Herbaceous"),
         X0.2=c("Burned", "Burned", "Burned", "Burned", "Burned", "Burned", "Burned"),
         X3.20=c("Closed coniferous", "Open coniferous", "Deciduous",
                 "Deciduous", "Deciduous", "Shrub", "Herbaceous"),
         X21.60=c("Closed coniferous", "Open coniferous", "Mixedwood",
                  "Mixedwood", "Deciduous", "Shrub", "Herbaceous"),
         X61.80=c("Closed coniferous", "Open coniferous", "Mixedwood",
                  "Mixedwood", "Deciduous", "Mixedwood", "Herbaceous"),
         X81.120=c("Closed coniferous", "Open coniferous", "Mixedwood",
                   "Mixedwood", "Deciduous", "Mixedwood", "Herbaceous"),
         X121.160=c("Closed coniferous", "Open coniferous", "Mixedwood",
                    "Open coniferous", "Deciduous", "Closed coniferous",
                    "Herbaceous"),
         X.160=c("Closed coniferous", "Open coniferous", "Closed coniferous",
                 "Closed coniferous", "Closed coniferous", "Closed coniferous",
                 "Herbaceous")),
    .Names=c("Veg.Type", "X0.2", "X3.20", "X21.60", "X61.80", "X81.120", "X121.160", "X.160"),
    class="data.frame", row.names=c(NA, -7L)) %>%
    as.tbl


  numYearsPer <- trajObj.raw %>%
    dplyr::select(-Veg.Type, -X.160) %>%
    colnames %>%
    sub(pattern = "X",replacement="", x = .) %>%
    strsplit(.,"\\.") %>%
    sapply(., function(x) diff(as.numeric(x)))

  maxAge <- 200L
  ages <- 0L:maxAge

  trajObj1 <- trajObj.raw %>%
    filter(Veg.Type != "Herbaceous") %>%
    dplyr::select(-Veg.Type) %>%
    apply(., 1, function(x) rep(x, times=c(numYearsPer, maxAge+1-sum(numYearsPer))))

  trajObj2 <- trajObj1 %>%
    cbind(matrix(rep(c("Burned", "Wetland", "Water", "Cropland","Other"),
                               each=maxAge+1), ncol=5))
  sim$trajObj <- matrix(fmatch(trajObj2, as.character(lcc05TrajReclass$Description)),
                     ncol=ncol(trajObj2))

  # Make a factor map, allowing for character labels
  sim$vegMapBeacons <- ratify(reclassify(sim$vegMapLcc, lcc05VegTable))
  levels(sim$vegMapBeacons)<-
    data.frame(ID=lcc05VegReclass$VEG.reclass, Class=lcc05VegReclass$Description) %>%
      .[levels(sim$vegMapBeacons)[[1]]$ID,]

  indices <- c(1,lcc05VegTable[,1][fmatch(1:11, lcc05VegTable[,2])]+1)
  setColors(sim$vegMapBeacons, n=12 ) <- getColors(sim$vegMapLcc)[[1]][indices]

  sim$trajMapBeacons <- reclassify(sim$vegMapLcc, lcc05TrajTable)
  setColors(sim$trajMapBeacons, n=12) <- colorRampPalette(brewer.pal(8, "Set1"))(12)

  return(invisible(sim))
}
