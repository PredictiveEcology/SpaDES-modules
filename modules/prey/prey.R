## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "prey",
  description = "prey model",
  keywords = "blah",
  authors = structure(list(list(given = "Jane", family = "Doe",
                                role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.7", prey = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "prey.Rmd")),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("K", "numeric", 500, NA, NA,
                    "Carrying capacity of prey"),
    defineParameter("r", "numeric", 0.1, NA, NA,
                    "Growth rate of prey"),
    defineParameter("beta", "numeric", 0.2, NA, NA,
                    "Predator effect on prey"),
    defineParameter("timeStep", "numeric", 1, NA, NA,
                    "Simulation time step")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "prey0", objectClass = "numeric",
                 desc = "Prey density at time 0", sourceURL = NA),
    expectsInput(objectName = "predPop", objectClass = "numeric",
                 desc = "Predator density at time 0", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "preyPop", objectClass = "numeric",
                  desc = "Vector of prey densities"),
    createsOutput(objectName = "preyConsumed", objectClass = "numeric",
                 desc = "Predator density at time 0", sourceURL = NA)
  )
))

doEvent.prey = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # initialise the pop. of prey
      sim$preyPop <- sim$prey0


      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$timeStep, "prey", "growPrey",
                           eventPriority = 5)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$timeStep, "prey", "predation",
                           eventPriority = 6)
      sim <- scheduleEvent(sim, end(sim), "prey", "plotPrey",
                           eventPriority = 7)

    },
    growPrey = {
      ## prey growth
      sim$preyPop[time(sim)] <- sim$preyPop[time(sim) - 1] *
        exp(P(sim)$r * (1 - sim$preyPop[time(sim) - 1] / P(sim)$K))

      if (sim$preyPop[time(sim)] < 0)
        sim$preyPop[time(sim)] <- 0

      sim <- scheduleEvent(sim, time(sim) + P(sim)$timeStep, "prey", "growPrey",
                           eventPriority = 5)

    },
    predation = {
      ## predation
      sim$preyConsumed <- P(sim)$beta*sim$preyPop[time(sim) - 1]*sim$predPop[time(sim) - 1]
      sim$preyPop[time(sim)] <- sim$preyPop[time(sim)] - sim$preyConsumed

      sim <- scheduleEvent(sim, time(sim) + P(sim)$timeStep, "prey", "predation",
                           eventPriority = 6)
    },
    plotPrey = {
      plotData <- data.table(N = sim$preyPop, time = 1:time(sim))
      plotPrey <- ggplot(plotData) +
        geom_line(aes(x = time, y = N), col = "blue")

      Plot(plotPrey, new = TRUE)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  if (!suppliedElsewhere("prey0", sim)) {
    sim$prey0 <- 1
  }

  if (!suppliedElsewhere("predPop", sim)) {
    sim$predPop <- rep(0, end(sim))
  }

  return(invisible(sim))
}
