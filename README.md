# SpaDES modules

A collection of available modules for use with the `SpaDES` R package ([link](http://spades.predictiveecology.org/)).

## Downloading modules from GitHub

The easiest way is to find the original GitHub location of the module you want to download and run this:

```
source("https://raw.githubusercontent.com/PredictiveEcology/SpaDES-modules/master/R/getModule.R")

```

## Downloading modules

The easiest way to use modules from this repository is using the built-in functionality in the `SpaDES` package:

    library(SpaDES)
    saveTo <- "~/SpaDES-modules" # change this to suit your needs
    downloadModule(name = "moduleName", path = saveTo)

This will download and unzip the module files in the directory speficied by `saveTo`.

## Example download -- Game of Life

    #    # install.packages("SpaDES")
    library(SpaDES)
    setPaths() # sets 4 paths that SpaDES uses to temporary locations
    # setPaths(modulePath = "~") # can try non-temporary locations too to keep between R sessions
    
    moduleName <- "gameOfLife"
    downloadModule(moduleName) # downloads to getPaths()$modulePath
    
    modules <- list(moduleName)
    
    ######################
    ## Blinking
    ######################
    
    X = c(10)
    Y = c(10)
    TYPE <- "blinker" ## see below for other types
    
    parameters <- list(
      gameOfLife = list(X = X, Y = Y, initialType = TYPE)
    )
    times <- list(start = 1, end = 30) # only run for 10 steps
    
    clearPlot()
    dev() # creates new, faster window if in RStudio
    mySim <- simInitAndSpades(times = times, params = parameters, modules = modules)
    
    ######################
    # Do random starts on bigger map
    ######################
    
    X = c(100)
    Y = c(100)
    TYPE <- NA ## random
    
    parameters <- list(
      gameOfLife = list(X = X, Y = Y, initialType = TYPE)
    )
    clearPlot()
    mySim <- simInitAndSpades(times = times, params = parameters, modules = modules)
    ####################
    ####################

    
#### Load documentation and source code

    ####################
    #  Load Documentation and Source Code
    ####################
    # Load the documentation .Rmd
    file.edit(file.path(getPaths()$modulePath, moduleName, paste0(moduleName, ".Rmd")))

    # Load the module source code .R
    file.edit(file.path(getPaths()$modulePath, moduleName, paste0(moduleName, ".R")))
                              


## [Known modules (existing or in development)](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development)

For a list of other modules currently in development, see [here](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development).

