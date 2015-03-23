# SpaDES modules

A collection of available modules for use with the `SpaDES` R package ([link](https://github.com/PredictiveEcology/SpaDES)).

## Downloading modules

The easiest way to use modules from this repository is using the built-in functionality in the `SpaDES` package:

    library(SpaDES)
    saveTo <- "~/SpaDES-modules" # change this to suit your needs
    downloadModule(name="moduleName", path=saveTo)

This will download and unzip the module files in the directory speficied by `saveTo`.
