stopifnot(packageVersion("SpaDES") >= "0.6.0")

### Specify module (and dependencies) definitions:
###
### name:         cropReprojectLccAge
###
### description:  <provide module description>
###
### keywords:     <provide module keywords>
###
### authors:      <author name(s) and email address(es)>
###
### version:      0.0.0
###
### spatialExtent: NA
###
### timeframe:    NA
###
### timestep:     NA
###
### translators:  NA
###
### citation:     NA
###
### reqdPkgs:     NA
###
### inputObjects: objectName: NA
###               objectClass: NA
###               other: NA
###
### outputObjects: objectName: NA
###                objectClass: NA
###                other: NA
###
### cropReprojectLccAge module metadata
defineModule(sim, list(
  name="cropReprojectLccAge",
  description="A translator module. Crops and reprojects the Land cover classification from 2005 to
  a smaller, cropped RasterLayer, defined by ext, and with new projection defined by newCRS",
  keywords=c("translator", "lcc05", "Land Cover Classification", "vegetation"),
  authors=c(person(c("Eliot", "J","B"), "McIntire", email="emcintir@nrcan.gc.ca", role=c("aut", "cre"))),
  version=numeric_version("0.0.1"),
  spatialExtent=raster::extent(rep(NA_real_, 4)),
  timeframe=as.POSIXlt(c(NA, NA)),
  timestep=NA_real_,
  citation=list(),
  reqdPkgs=list("raster","rgeos"),
  parameters=rbind(
    defineParameter(".plotInitialTime", "numeric", NA_real_),
    defineParameter(".plotInterval", "numeric", NA_real_),
    defineParameter(".saveInitialTime", "numeric", NA_real_),
    defineParameter(".saveInterval", "numeric", NA_real_)),
  inputObjects=data.frame(objectName=c("lcc05", "age", ".shinyPolygon"),
                          objectClass=c("RasterLayer", "RasterLayer", "SpatialPolygons"),
                          other=rep(NA_character_, 3L), stringsAsFactors=FALSE),
  outputObjects=data.frame(objectName=c("vegMapLcc", "ageMapInit"),
                          objectClass=c("RasterLayer", "RasterLayer"),
                          other=rep(NA_character_, 2L), stringsAsFactors=FALSE)
#   parameters=rbind(
#     defineParameter("paramName", "paramClass", value),
#     defineParameter("paramName", "paramClass", value)),
#   inputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=list(NA)),
#   outputObjects=data.frame(objectName=NA_character_, objectClass=NA_character_, other=list(NA))
))

### event functions:
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - module name and this filename must match;
#   - keep event functions short and clean, modularize by calling
#       subroutines from section below.

### template event
doEvent.cropReprojectLccAge = function(sim, eventTime, eventType, debug=FALSE) {
  if (eventType=="init") {
    ### check for more detailed object dependencies:
    ### (use `checkObject` or similar)

    # do stuff for this event
    sim <- cropReprojectLccInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, simParams(sim)$cropReprojectLccAge$.plotInitialTime, "cropReprojectLccAge", "plot")
    sim <- scheduleEvent(sim, simParams(sim)$cropReprojectLccAge$.saveInitialTime, "cropReprojectLccAge", "save")
  } else if (eventType=="templateEvent") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event

    # e.g., call your custom functions/methods here
    # you can define your own methods below this `doEvent` function

    # schedule future event(s)

    # e.g.,
    # sim <- scheduleEvent(sim, simCurrentTime(sim) + increment, "cropReprojectLccAge", "templateEvent")

    # ! ----- STOP EDITING ----- ! #
    } else {
      warning(paste("Undefined event type: '", simEvents(sim)[1, "eventType", with=FALSE],
                    "' in module '", simEvents(sim)[1, "moduleName", with=FALSE], "'", sep=""))
    }
  return(invisible(sim))
}

### template initilization
cropReprojectLccInit = function(sim) {

  #    a <- .shinyPolygon@polygons[[1]]@Polygons[[1]]@coords
  #    .shinyPolygon@polygons[[1]]@Polygons[[1]]@coords[,2] <- a[,1]
  #    .shinyPolygon@polygons[[1]]@Polygons[[1]]@coords[,1] <- a[,2]
  #    
  #   crs(.shinyPolygon)  <- CRS("+proj=longlat +datum=WGS84")
  #   assignGlobal(".shinyPolygon", .shinyPolygon)
    
  #   Sr1 = Polygon(cbind(c(-106,-103,-105.5,-105.8,-106),c(746,747,748,747,746))*1e4)
  #   Srs1 = Polygons(list(Sr1), "s1")
  #   shinyPolygon <- SpatialPolygons(list(Srs1), 1L)  
  #   crs(SpP) <- crs(vegMapLcc)
  #   assignGlobal("SpP", SpP)
  #   vegMapLcc <- crop(getGlobal("lcc05"), SpP)
  #   Plot(vegMapLcc, new=T)
  #   Plot(SpP, addTo="vegMapLcc$LCC2005_V1_4a")
  #   vegMapLcc <- mask(vegMapLcc, SpP)
  #   assignGlobal("vegMapLcc", vegMapLcc)
  #   Plot(vegMapLcc, new=T)
  #   Plot(SpP, addTo="vegMapLcc$LCC2005_V1_4a")
  #   
    
  lcc05CRS <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80
    +units=m +no_defs")
  .shinyPolygonLcc05 <- spTransform(getGlobal(".shinyPolygon"), CRSobj =lcc05CRS)
  totalArea <- rgeos::gArea(.shinyPolygonLcc05)/1e4
  if(totalArea > 2e6) stop("In the current implementation, please select another, smaller polygon (less than 2 million hectares)")
  shinyPolygon <- .shinyPolygonLcc05
  
  vegMapLcc <- crop(getGlobal("lcc05"), shinyPolygon)
  crs(vegMapLcc) <- lcc05CRS
  vegMapLcc <- mask(vegMapLcc, shinyPolygon)
  setColors(vegMapLcc, n=256)<-getColors(lcc05)[[1]] # mask removes colors!
    
  if(ncell(vegMapLcc)>5e5) beginCluster(6)
  
  # age will not run with projectRaster directly. Instead, project the vegMap to age, then crop, then project back to vegMap
  vegMapLcc.crsAge <- projectRaster(vegMapLcc, crs=crs(getGlobal("age")))
  age.crsAge <- crop(getGlobal("age"), spTransform(getGlobal(".shinyPolygon"), 
                                                                  CRSobj = crs(getGlobal("age"))))
  age.crsAge <- mask(age.crsAge, spTransform(getGlobal(".shinyPolygon"), 
                                                   CRSobj = crs(getGlobal("age"))))
  ageMapInit <- projectRaster(age.crsAge, to=vegMapLcc, method="ngb")
  
  setColors(ageMapInit) <- colorRampPalette(c("light green","dark green"))(50)
  assignGlobal("vegMapLcc", vegMapLcc)
  assignGlobal("ageMapInit", ageMapInit)
  
  endCluster()

  return(invisible(sim))
}
