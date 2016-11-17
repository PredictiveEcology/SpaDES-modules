# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "gameOfLife",
  description = "An implementation of Conway's Game of Life in SpaDES.",
  keywords = c("cellular automata", "game of life"),
  authors = c(person(c("Alex", "M"), "Chubaty", email = "alexander.chubaty@canada.ca", role = c("aut", "cre"))),
  childModules = character(0),
  version = numeric_version("0.0.2"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "gameOfLife.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter("X", "integer", 1000L, NA, NA, "the number of columns in the grid/raster."),
    defineParameter("Y", "integer", 1000L, NA, NA, "the number of rows in the grid/raster."),
    defineParameter("seed", "logical", NA, NA, NA, "a logical vector to be used to initialize the grid/raster.")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = NA, objectClass = NA, desc = NA, sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = 'world', objectClass = 'RasterLayer', desc = 'A raster grid of cells.')
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.gameOfLife = function(sim, eventTime, eventType, debug = FALSE) {
  if (eventType == "init") {
    # do stuff for this event
    sim <- sim$gameOfLifeInit(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "gameOfLife", "plot")
    sim <- scheduleEvent(sim, time(sim) + 1, "gameOfLife", "generation")
  } else if (eventType == "plot") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    Plot(sim$world)

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "gameOfLife", "plot")
    # ! ----- STOP EDITING ----- ! #
  } else if (eventType == "generation") {
    # ! ----- EDIT BELOW ----- ! #
    # do stuff for this event
    sim <- sim$gameOfLifeGeneration(sim)

    # schedule future event(s)
    sim <- scheduleEvent(sim, time(sim) + 1, "gameOfLife", "generation")
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
gameOfLifeInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  if (length(P(sim)$seed) != P(sim)$X*P(sim)$Y) {
    stop("gameOfLifeInit: seed must be of length X*Y")
  }
  sim$world <- raster(extent(0, P(sim)$X, 0, P(sim)$Y),
                      ncols = P(sim)$X, nrows = P(sim)$Y, vals = P(sim)$seed)
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

gameOfLifeGeneration <- function(sim) {
  w <- matrix(c(1,1,1,1,0,1,1,1,1), ncol = 3, nrow = 3)
  r <- sim$world
  f <- focal(r, w, fun = sum, na.rm = TRUE, pad = TRUE)
  
  # Any live cell with fewer than two live neighbours dies.
  sim$world[r*f < 2] <- FALSE

  # Any live cell with two or three live neighbours lives on to the next generation.
  sim$world[(r*f == 2) | (r*f == 3)] <- TRUE

  # Any live cell with more than three live neighbours dies, as if by over-population.
  sim$world[r*f > 3] <- FALSE

  # Any dead cell with exactly three live neighbours becomes a live cell.
  sim$world[(!r)*f == 3] <- TRUE
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
