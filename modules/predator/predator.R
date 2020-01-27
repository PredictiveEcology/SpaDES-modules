## Everything in this file gets sourced during `simInit()`,
## and all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used without `sim$` as they are namespaced to the module,
## just like functions in R packages.
## If exact location is required, functions will be: `sim$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "predator",
  description = "predator model",
  keywords = "blah",
  authors = structure(list(list(given = "Jane", family = "Doe",
                                role = c("aut", "cre"), email = "email@example.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.7", predator = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "predator.Rmd")),
  reqdPkgs = list(),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("delta", "numeric", 0.5, NA, NA,
                    "Predators uptake efficiency"),
    defineParameter("gamma", "numeric", 0.1, NA, NA,
                    "Prey death rate in absence of prey"),
    defineParameter("timeStep", "numeric", 1, NA, NA,
                    "Simulation time step")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "pred0", objectClass = "numeric",
                 desc = "Predator density at time 0", sourceURL = NA),
    expectsInput(objectName = "preyConsumed", objectClass = "numeric",
                 desc = "Predator density at time 0", sourceURL = NA),
    expectsInput(objectName = "preyPop", objectClass = "numeric",
                 desc = "Predator density at time 0", sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "predPop", objectClass = "numeric",
                  desc = "Vector of predator densities")
  )
))

doEvent.predator = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # initialise the pop. of predators
      sim$predPop <- sim$pred0

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$timeStep, "predator", "growPred",
                           eventPriority = 7)
      sim <- scheduleEvent(sim, end(sim), "predator", "plotPred",
                           eventPriority = 8)
    },
    growPred = {
      sim$predPop[time(sim)] <- sim$predPop[time(sim) - 1] +
        P(sim)$delta*sim$preyConsumed - P(sim)$gamma*sim$predPop[time(sim) - 1]

      if (sim$predPop[time(sim)] < 0)
        sim$predPop[time(sim)] <- 0

      sim <- scheduleEvent(sim, time(sim) + P(sim)$timeStep, "predator", "growPred",
                           eventPriority = 7)

    },
    plotPred = {
      plotData <- data.table(N = sim$predPop, time = 1:time(sim))
      plotPred <- ggplot(plotData) +
        geom_line(aes(x = time, y = N), col = "red")

      Plot(plotPred, new = TRUE)
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  if (!suppliedElsewhere("pred0", sim)) {
    sim$pred0 <- 2
  }

  if (!suppliedElsewhere("preyConsumed", sim)) {
    sim$preyConsumed <- runif(1, 1, 2)
  }

  if (!suppliedElsewhere("preyPop", sim)) {
    sim$preyPop <- rep(10, end(sim))
  }

  return(invisible(sim))
}
