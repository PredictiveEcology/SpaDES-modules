# SpaDES modules

A collection of available modules for use with the `SpaDES` R package ([link](http://spades.predictiveecology.org/)).

## Downloading modules

The easiest way to use modules from this repository is using the built-in functionality in the `SpaDES` package:

    library(SpaDES)
    saveTo <- "~/SpaDES-modules" # change this to suit your needs
    downloadModule(name = "moduleName", path = saveTo)

This will download and unzip the module files in the directory speficied by `saveTo`.

## [Known modules (existing or in development)](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development)

For a list of other modules currently in development, see [here](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development).
