# SpaDES modules

**This is the catalogue of SpaDES modules.** It is a place to find pieces of
model code that other people have already written, and a place to list your own
so that others can find them.

### 👉 [Browse the module list](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development)

The list is grouped by subject — forests, fire, carbon, wildlife, disturbance,
climate data, and so on. Each entry gives the name of the people who wrote it and
a link to its code.

Almost every module lives in its own GitHub repository, not in this one. You do
not need to download anything from here.

## New to SpaDES? Start here

- **[Robust and nimble scientific workflows, using SpaDES](https://predictiveecology.org/training/_book/)** —
  the book. How to install R and the packages, how to set up your first project,
  and full worked examples you can copy and run.
- **[SpaDES.project](https://spades-project.predictiveecology.org)** — the
  package that builds a project for you: it fetches the modules you asked for,
  installs the R packages they need, and makes the folders.

## How you use a module

You give `setupProject()` the GitHub locations of the modules you want. It gets
them and everything they depend on, then you run them.

```r
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
options(repos = repos)
if (!require("pak")) install.packages("pak")
pak::pak("SpaDES.project", ask = FALSE)

out <- SpaDES.project::setupProject(
  paths = list(projectPath = "~/myFirstProject"),
  modules = c(
    "PredictiveEcology/Biomass_speciesData@main",
    "PredictiveEcology/Biomass_borealDataPrep@main",
    "PredictiveEcology/Biomass_core@main"
  ),
  times = list(start = 2011, end = 2031)
)

simOut <- SpaDES.core::simInitAndSpades2(out)
```

Use any entry from the module list in `modules`. The part after `@` is the
branch, and you can leave it off. Some repositories hold several modules in
folders, so you point at the folder:

```r
modules = file.path("PredictiveEcology/scfm@development/modules",
                    c("scfmLandcoverInit", "scfmRegime", "scfmDriver",
                      "scfmIgnition", "scfmEscape", "scfmSpread"))
```

The book explains all of this in more detail:
[Setting up a project](https://predictiveecology.org/training/_book/setupProject.html).

## Writing your own module

The book walks through it, starting at
[Creating a new module](https://predictiveecology.org/training/_book/NewModuleIntro.html).
In short, `SpaDES.core::newModule("myModule", path = "modules")` writes the
skeleton files for you, and you fill in the parts that do the work.

## Adding your module to the list

Put your module in a public GitHub repository, then either

- edit the [module list](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development)
  yourself (it is a wiki page, anyone can edit it), or
- [open an issue](https://github.com/PredictiveEcology/SpaDES-modules/issues)
  telling us the repository name and what the module does.

## What else is in this repository

A handful of small, old demonstration modules sit in `modules/` (for example
`gameOfLife` and `LCC2005`). They are kept for teaching and for historical
reasons, and are no longer maintained. Anything you are likely to want for real
work is in the [module list](https://github.com/PredictiveEcology/SpaDES-modules/wiki/Current-modules-in-development).

## Where to get help

- [predictiveecology.org](https://predictiveecology.org) — news, projects,
  manuals, workshops
- [SpaDES package documentation](https://spades.predictiveecology.org)
- [Ask a question or report a problem](https://github.com/PredictiveEcology/SpaDES-modules/issues)
