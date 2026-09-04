# Known SpaDES modules

A subject-by-subject list of SpaDES modules that are publicly available on GitHub.
Each entry links to the module's own repository, and names the people who wrote it.

New to SpaDES? Start with the book,
[Robust and nimble scientific workflows, using SpaDES](https://predictiveecology.org/training/_book/),
and with [SpaDES.project](https://spades-project.predictiveecology.org).

_Generated %GENERATED%. Something missing or wrong? Edit this page, or
[open an issue](https://github.com/PredictiveEcology/SpaDES-modules/issues)._

## How to get a module

You do not download modules by hand. Give `setupProject()` the GitHub locations
and it fetches them, plus the R packages they need:

```r
repos <- c("https://predictiveecology.r-universe.dev", getOption("repos"))
options(repos = repos)
if (!require("pak")) install.packages("pak")
pak::pak("SpaDES.project", ask = FALSE)

out <- SpaDES.project::setupProject(
  paths = list(projectPath = "~/myProject"),
  modules = c(
    "PredictiveEcology/Biomass_speciesData@main",
    "PredictiveEcology/Biomass_borealDataPrep@main",
    "PredictiveEcology/Biomass_core@main"
  ),
  times = list(start = 2011, end = 2031)
)

simOut <- SpaDES.core::simInitAndSpades2(out)
```

The part after `@` is the branch name; you can leave it off. A few repositories
hold several modules in folders, so you point at the folder:

```r
modules = file.path("PredictiveEcology/scfm@development/modules",
                    c("scfmLandcoverInit", "scfmRegime", "scfmDriver",
                      "scfmIgnition", "scfmEscape", "scfmSpread"))
```

**Status markers**

| | meaning |
|---|---|
| 🟢 | **active** — changed in the last 12 months on its live branch |
| 🔵 | **stable** — no recent changes, but a project active in the last 18 months still uses it |
| ⚪ | **quiet** — no recent changes and no public project known to use it |
| 🗄 | repository is archived (read-only) |
| ⚠ | still uses the retired `raster` / `sp` packages, so may need work to run on a current stack |
| ·N | number of public projects that list this module |

**Which accounts were scanned.** Every public repository in these eight accounts was
checked (a repository counts as a module if it has a `<name>.R` at its root):

%SCAN_TABLE%

Forks were dropped where the upstream is itself in this list, which is why the list is
shorter than 207. The CASTOR section came from searching [bcgov](https://github.com/bcgov)
for `castor` by name — the bcgov organisation as a whole was **not** scanned. Nor were
the accounts under "Modules elsewhere". If your modules live somewhere else, say so and
they will be added.

How these were worked out: activity is counted on each repository's *live* branch
(`development` where it exists, otherwise the default branch) — many default branches
here are years behind. Usage comes from scanning the driver scripts (`global.R` and
similar) of %N_PROJECT_REPOS% public project repositories for module names. ⚠ is set only where the
code genuinely calls `raster::` / `sp::` or uses `Raster*` / `Spatial*` classes, not
merely where `reqdPkgs` lists them.

**⚪ means "no public user found", not "abandoned"** — private and personal project
repositories are invisible to this scan, so a module used only in those will look quiet.

- Only public repositories are listed. Some groups also have private modules —
  write to the people named to ask about access.

---

## Forest vegetation dynamics: LandR Biomass

A forest succession model, originally derived from LANDIS-II Biomass Succession.
`Biomass_core` is the engine; the others prepare its inputs or summarise its output.

_Eliot McIntire, Yong Luo, Ceres Barros, Alex Chubaty, Ian Eddy, Jean Marchal_

- [Biomass_core](https://github.com/PredictiveEcology/Biomass_core) — the succession engine
- [Biomass_borealDataPrep](https://github.com/PredictiveEcology/Biomass_borealDataPrep) — parameterises the model for the Canadian boreal forest
- [Biomass_speciesData](https://github.com/PredictiveEcology/Biomass_speciesData) — builds the tree species cover layers
- [Biomass_speciesParameters](https://github.com/PredictiveEcology/Biomass_speciesParameters) — fits species growth parameters (_Ian Eddy, Eliot McIntire, Ceres Barros_)
- [Biomass_speciesFactorial](https://github.com/PredictiveEcology/Biomass_speciesFactorial) — factorial experiment over species traits (_Eliot McIntire_)
- [Biomass_regeneration](https://github.com/PredictiveEcology/Biomass_regeneration) — post-disturbance regeneration
- [Biomass_regenerationPM](https://github.com/PredictiveEcology/Biomass_regenerationPM) — as above, with partial mortality (_Ceres Barros_)
- [Biomass_yieldTables](https://github.com/PredictiveEcology/Biomass_yieldTables) — turns simulated growth into yield tables (_Céline Boisvenue, Dominique Caron, Camille Giuliano, Eliot McIntire_)
- [Biomass_validationKNN](https://github.com/PredictiveEcology/Biomass_validationKNN) — validation against kNN forest maps (_Ceres Barros, Eliot McIntire_)
- [Biomass_summary](https://github.com/PredictiveEcology/Biomass_summary) — summarises many runs, study areas and scenarios (_Alex Chubaty, Tati Micheletti, Ian Eddy_)

### Growth, mortality and regeneration add-ons

- [LandR_reforestation](https://github.com/ianmseddy/LandR_reforestation) (_Ian Eddy_)
- [gmcsDataPrep](https://github.com/ianmseddy/gmcsDataPrep) — climate-sensitive growth and mortality inputs from permanent sample plots (_Ian Eddy_)
- [PSP_Clean](https://github.com/ianmseddy/PSP_Clean) — cleans permanent sample plot data (_Ian Eddy_)
- [assistedMigrationBC](https://github.com/ianmseddy/assistedMigrationBC) (_Ian Eddy_)
- [LandR_BiomassGMCC](https://github.com/ianmseddy/LandR_BiomassGMCC) — climate-sensitive growth and mortality (_Yong Luo_)
- [LandR_BiomassGMOrig](https://github.com/eliotmcintire/LandR_BiomassGMOrig) — the original growth and mortality module
- [LBMR2LCC_DataPrep](https://github.com/PredictiveEcology/LBMR2LCC_DataPrep) — translates LBMR output to land cover classes (_Jean Marchal_)

---

## Fire

### fireSense

A climate-sensitive statistical fire model: ignition, escape and spread are each
fitted to data, then predicted forward.

_Eliot McIntire, Ian Eddy, Jean Marchal, Tati Micheletti, Alex Chubaty, Ceres Barros_

- [fireSense](https://github.com/PredictiveEcology/fireSense) — the simulation module (_Eliot McIntire, Jean Marchal_)
- [fireSense_dataPrepFit](https://github.com/PredictiveEcology/fireSense_dataPrepFit) (_Ian Eddy, Eliot McIntire_)
- [fireSense_dataPrepPredict](https://github.com/PredictiveEcology/fireSense_dataPrepPredict) (_Ian Eddy, Eliot McIntire, Alex Chubaty_)
- [fireSense_IgnitionFit](https://github.com/PredictiveEcology/fireSense_IgnitionFit) (_Eliot McIntire, Ian Eddy, Jean Marchal_)
- [fireSense_IgnitionPredict](https://github.com/PredictiveEcology/fireSense_IgnitionPredict) (_Eliot McIntire, Ian Eddy, Jean Marchal_)
- [fireSense_EscapeFit](https://github.com/PredictiveEcology/fireSense_EscapeFit) (_Eliot McIntire, Ian Eddy, Jean Marchal_)
- [fireSense_EscapePredict](https://github.com/PredictiveEcology/fireSense_EscapePredict) (_Eliot McIntire, Ian Eddy, Jean Marchal_)
- [fireSense_SpreadFit](https://github.com/PredictiveEcology/fireSense_SpreadFit) (_Eliot McIntire, Tati Micheletti, Ian Eddy, Jean Marchal_)
- [fireSense_SpreadPredict](https://github.com/PredictiveEcology/fireSense_SpreadPredict) (_Eliot McIntire, Tati Micheletti, Ian Eddy, Jean Marchal_)
- [fireSense_ELFs](https://github.com/PredictiveEcology/fireSense_ELFs) — ecologically-based low fractal dimension polygons (_Eliot McIntire, Ian Eddy_)
- [fireSense_hindcast](https://github.com/PredictiveEcology/fireSense_hindcast) — run fireSense over historic climate years (_Alex Chubaty_)
- [fireSense_summary](https://github.com/PredictiveEcology/fireSense_summary) (_Alex Chubaty, Tati Micheletti, Ian Eddy_)
- [fireWeather](https://github.com/CeresBarros/fireWeather) — summarises weather data for fire modelling (_Ceres Barros_)
- [fireSense_dataPrep](https://github.com/CeresBarros/fireSense_dataPrep) — weather, fire and fuel data prep (_Ceres Barros_)

Older members of the family, all by _Jean Marchal_ and colleagues:

- [fireSense_SizeFit](https://github.com/PredictiveEcology/fireSense_SizeFit)
- [fireSense_SizePredict](https://github.com/PredictiveEcology/fireSense_SizePredict)
- [fireSense_NWT](https://github.com/PredictiveEcology/fireSense_NWT)
- [fireSense_NWT_DataPrep](https://github.com/PredictiveEcology/fireSense_NWT_DataPrep)
- [MDC_NWT_DataPrep](https://github.com/PredictiveEcology/MDC_NWT_DataPrep)
- [fireSense_dataPrep](https://github.com/PredictiveEcology/fireSense_dataPrep)

### SCFM — Steve Cumming Fire Model

Ignition, escape and spread, calibrated from a fire regime.

**[PredictiveEcology/scfm](https://github.com/PredictiveEcology/scfm)** %SCFM%

_Steve Cumming, Ian Eddy, Eliot McIntire, Alex Chubaty_

This is **one repository**, not eleven modules. Everything below is a folder inside
it, released and maintained together, so they all carry the repository's status
shown above rather than a status of their own. Within it, `raster`/`sp` survive in
`scfmLandcoverInit`, `scfmDriver` and the two `andisonDriver` modules.

- [scfmLandcoverInit](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmLandcoverInit)
- [scfmDataPrep](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmDataPrep)
- [scfmRegime](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmRegime)
- [scfmDriver](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmDriver)
- [scfmIgnition](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmIgnition)
- [scfmEscape](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmEscape)
- [scfmSpread](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmSpread)
- [scfmDiagnostics](https://github.com/PredictiveEcology/scfm/tree/development/modules/scfmDiagnostics)
- [ageModule](https://github.com/PredictiveEcology/scfm/tree/development/modules/ageModule)
- [group_scfm](https://github.com/PredictiveEcology/scfm/tree/development/modules/group_scfm) — runs the whole set as one
- [andisonDriver](https://github.com/PredictiveEcology/scfm/tree/master/modules/andisonDriver) and [andisonDriver_dataPrep](https://github.com/PredictiveEcology/scfm/tree/master/modules/andisonDriver_dataPrep) — on `master` only


### Other fire modules

- [LandMine](https://github.com/PredictiveEcology/LandMine) — landscape fire simulator driven by fire return intervals (_Eliot McIntire, Alex Chubaty_)
- [FavierFireSpread](https://github.com/CeresBarros/FavierFireSpread) — percolation-based fire spread for LandR (_Ceres Barros_)
- [historicFires](https://github.com/PredictiveEcology/historicFires) — raster layers of past fires (_Alex Chubaty, Christopher Mallon, Ian Eddy_)
- [canFireRegimeZones](https://github.com/PredictiveEcology/canFireRegimeZones) — fire regime polygons after Erni et al. (2020) (_Alex Chubaty_)
- [burnSummaries](https://github.com/PredictiveEcology/burnSummaries) — burn maps and time-since-fire from simulation output (_Alex Chubaty_)

### Fuels and fire behaviour

- [Biomass_fuelsPFG](https://github.com/PredictiveEcology/Biomass_fuelsPFG) — fuels from plant functional groups (_Ceres Barros, Eliot McIntire, Steve Cumming_)
- [fireProperties](https://github.com/PredictiveEcology/fireProperties) — fire behaviour using the Canadian FBP System (_Ceres Barros_)
- [Biomass_fuels](https://github.com/PredictiveEcology/Biomass_fuels) — LANDIS-II Dynamic Biomass Fuels (_Ceres Barros_)

---

## Forest carbon

### CBM — Carbon Budget Model

A SpaDES implementation of the Canadian Forest Service carbon budget model.
Project repository: [spadesCBM](https://github.com/PredictiveEcology/spadesCBM).

_Céline Boisvenue, Camille Giuliano, Susan Murray, Alex Chubaty, Dominique Caron_

- [CBM_core](https://github.com/PredictiveEcology/CBM_core)
- [CBM_defaults](https://github.com/PredictiveEcology/CBM_defaults)
- [CBM_dataPrep](https://github.com/PredictiveEcology/CBM_dataPrep)
- [CBM_dataPrep_SK](https://github.com/PredictiveEcology/CBM_dataPrep_SK) — Saskatchewan
- [CBM_dataPrep_RIA](https://github.com/PredictiveEcology/CBM_dataPrep_RIA) — northeast BC
- [CBM_vol2biomass](https://github.com/PredictiveEcology/CBM_vol2biomass) — volume to biomass conversion

Northeast BC (RIA) scenario variants, by _Céline Boisvenue_ and _Alex Chubaty_:

- [CBM_dataPrep_RIApresentDay](https://github.com/cboisvenue/CBM_dataPrep_RIApresentDay)
- [CBM_dataPrep_RIAfri](https://github.com/cboisvenue/CBM_dataPrep_RIAfri)
- [CBM_dataPrep_RIAharvest1](https://github.com/cboisvenue/CBM_dataPrep_RIAharvest1)
- [CBM_dataPrep_RIAharvest2](https://github.com/cboisvenue/CBM_dataPrep_RIAharvest2)
- [CBM_vol2biomass_RIA](https://github.com/cboisvenue/CBM_vol2biomass_RIA)

### Linking vegetation and carbon

- [LandRCBM_split3pools](https://github.com/PredictiveEcology/LandRCBM_split3pools) — splits LandR biomass into CBM carbon pools (_Céline Boisvenue, Dominique Caron, Susan Murray, Camille Giuliano, Alex Chubaty_). Collection repository: [LandRCBM](https://github.com/PredictiveEcology/LandRCBM)

### BiomeBGC

A process-based ecosystem carbon and water model.

_Dominique Caron, Céline Boisvenue, Alex Chubaty_

- [BiomeBGC_core](https://github.com/PredictiveEcology/BiomeBGC_core)
- [BiomeBGC_dataPrep](https://github.com/PredictiveEcology/BiomeBGC_dataPrep)
- [BiomeBGC_validationFluxTower](https://github.com/PredictiveEcology/BiomeBGC_validationFluxTower)
- [BGC](https://github.com/PredictiveEcology/BGC) — runs BiomeBGC from R (_Alex Chubaty, Céline Boisvenue_)

---

## Harvesting and forest management

- [simpleHarvest](https://github.com/ianmseddy/simpleHarvest) — simple spatially explicit harvest, interfaces with LandR (_Ian Eddy, Parvin Kalantari_)
- [simpleHarvestPlanning](https://github.com/PredictiveEcology/simpleHarvestPlanning) (_Ian Eddy, Parvin Kalantari_)
- [spades_ws3](https://github.com/PredictiveEcology/spades_ws3) — wrapper around the ws3 wood supply model (_Greg Paradis_; original at [UBC-FRESH/spades_ws3](https://github.com/UBC-FRESH/spades_ws3))
- [spades_ws3_dataInit](https://github.com/PredictiveEcology/spades_ws3_dataInit) — data setup for ws3 (_Greg Paradis, Ian Eddy, Eliot McIntire_)
- [spades_ws3_landrAge](https://github.com/ianmseddy/spades_ws3_landrAge) — keeps ws3 and LandR stand ages in step (_Ian Eddy_)
- [historicalFireAndHarvest](https://github.com/ianmseddy/historicalFireAndHarvest) (_Ian Eddy_)
- [forestHarvest-SpaDESmodule](https://github.com/CeresBarros/forestHarvest-SpaDESmodule) — a toy harvest model, good for learning (_Ceres Barros_)

---

## CASTOR: BC government forest and land management

A decision-support model for forest management and caribou habitat in British
Columbia, built by the BC Ministry of Forests. Unlike the SCFM set, each module is
its **own repository** under [bcgov](https://github.com/bcgov), and they are run together from the parent
project **[bcgov/castor](https://github.com/bcgov/castor)** %CASTOR_PARENT%.

_Kyle Lochhead, Tyler Muhly, Elizabeth Kleynhans_

Modules read 🔵 because the parent project is active (last change %CASTOR_PUSHED%) while
the individual module repositories mostly are not. ·N counts are not shown here: the
usage scan behind them covers the PredictiveEcology orbit, not bcgov, so a count would
not be comparable with the rest of this page. Every CASTOR module still uses the
retired `raster`/`sp` stack.

- [dataCastor](https://github.com/bcgov/dataCastor) — builds and connects `castordb`, the SQLite database of forest state and zone constraints that the other modules read and update
- [growingStockCastor](https://github.com/bcgov/growingStockCastor) — interpolates yield curves to update volume and ages the forest each time step
- [forestryCastor](https://github.com/bcgov/forestryCastor) — harvest scheduling; explores the decision space for forestry and caribou impacts
- [blockingCastor](https://github.com/bcgov/blockingCastor) — aggregates pixels into homogeneous cutblocks, by graph segmentation and agglomerative clustering
- [roadCastor](https://github.com/bcgov/roadCastor) — simulates strategic road networks as a single target access problem, after Anderson and Nelson (2004)
- [disturbanceCastor](https://github.com/bcgov/disturbanceCastor) — cumulative disturbance by zone, designed for forestry and caribou habitat
- [survivalCastor](https://github.com/bcgov/caribouSurvivalCastor) — adult female caribou survival in herd ranges, after Wittmer et al. (2007) — note the repository is `caribouSurvivalCastor`
- [fireCastor](https://github.com/bcgov/fireCastor) — wildfire simulation (_Elizabeth Kleynhans_)
- [climateCastor](https://github.com/bcgov/climateCastor) — retrieves climate data via `climR` for any area in the province
- [backCastor](https://github.com/bcgov/backCastor) — reconstructs the locations of cutblocks harvested in BC over roughly the past 50 years
- [volumebyareaReportCastor](https://github.com/bcgov/volumeReportCastor) — reports harvested volume by area of interest through time — note the repository is `volumeReportCastor`
- [uploadCastor](https://github.com/bcgov/uploadCastor) — uploads model outputs to a database

---

## Insects and forest health

### Mountain pine beetle

The Red Top model: short-run potential for beetle establishment, eruption and spread.

_Alex Chubaty, Barry Cooke, Eliot McIntire_ · project repository: [LandR_MPB](https://github.com/achubaty/LandR_MPB)

- [mpbClimateData](https://github.com/achubaty/mpbClimateData)
- [mpbMassAttacksData](https://github.com/achubaty/mpbMassAttacksData)
- [mpbPine](https://github.com/achubaty/mpbPine)
- [mpbRedTopSpread](https://github.com/achubaty/mpbRedTopSpread)
- [mpbRedTopGrowth](https://github.com/achubaty/mpbRedTopGrowth)
- [mpbRandomLandscapes](https://github.com/achubaty/mpbRandomLandscapes)
- [LandR_MPB_studyArea](https://github.com/achubaty/LandR_MPB_studyArea)

### Spruce budworm

_Alex Chubaty_ · project repository: [SBW_EasternBoreal](https://github.com/FOR-CAST/SBW_EasternBoreal)

- [SBW_dataPrep](https://github.com/FOR-CAST/SBW_dataPrep)
- [SBW_recruitment](https://github.com/FOR-CAST/SBW_recruitment)
- [SBW_dispersal](https://github.com/FOR-CAST/SBW_dispersal)
- [SBW_defoliation](https://github.com/FOR-CAST/SBW_defoliation)
- [SBW_naturalEnemies](https://github.com/FOR-CAST/SBW_naturalEnemies)

---

## Caribou

### Habitat and population

_Tati Micheletti, Frances Stewart, Eliot McIntire_

- [caribouRSF](https://github.com/tati-micheletti/caribouRSF) — resource selection function
- [caribouRSF_NT](https://github.com/tati-micheletti/caribouRSF_NT) — DeMars et al. (2019) RSF for the Northwest Territories (_Tati Micheletti_)
- [caribouIK](https://github.com/tati-micheletti/caribouIK) — habitat suitability from Indigenous Knowledge
- [caribouCIP](https://github.com/tati-micheletti/caribouCIP) — co-informed predictions, combining RSF and Indigenous Knowledge
- [caribouPopGrowthModel](https://github.com/tati-micheletti/caribouPopGrowthModel) — boreal caribou population growth
- [caribouPopGrowth_disturbance](https://github.com/tati-micheletti/caribouPopGrowth_disturbance) — links disturbance layers to the growth model (_Tati Micheletti_)
- [HSI_Caribou_MB](https://github.com/FOR-CAST/HSI_Caribou_MB) — Manitoba habitat suitability from NRV simulations (_Alex Chubaty_)
- [caribouNT_studyArea](https://github.com/PredictiveEcology/caribouNT_studyArea) (_Ian Eddy, Alex Chubaty, Eliot McIntire, Tati Micheletti_)

### Movement and telemetry

_Julie Turner, Rory McInnes, Tati Micheletti, Eliot McIntire_

- [caribouLocPrep](https://github.com/tati-micheletti/caribouLocPrep) — cleans and harmonises location data across jurisdictions
- [prepTracks](https://github.com/tati-micheletti/prepTracks) — builds tracks from GPS locations
- [prepLandscape](https://github.com/tati-micheletti/prepLandscape) (_Julie Turner_)
- [extractLand](https://github.com/tati-micheletti/extractLand) — extracts landscape values at points
- [caribouiSSA](https://github.com/tati-micheletti/caribouiSSA) — integrated step selection analysis
- [caribou_SSUD](https://github.com/FOR-CAST/caribou_SSUD) — steady-state utilization distribution (_Julie Turner_)
- [caribouNN](https://github.com/tati-micheletti/caribouNN) — movement model fitted with neural networks (_Tati Micheletti_)
- [caribouNN_Global](https://github.com/tati-micheletti/caribouNN_Global) (_Tati Micheletti_)
- [movementDataPrep](https://github.com/tati-micheletti/movementDataPrep) — pulls movement data from Movebank (_Tati Micheletti_)

---

## Birds

_Tati Micheletti, Diana Stralberg, Alex Chubaty, Isolde Lane-Shaw, Sourav Das_

- [birdsNWT](https://github.com/tati-micheletti/birdsNWT) — boreal songbird density models
- [birds_BRT](https://github.com/FOR-CAST/birds_BRT) — boosted regression tree bird models for NWT and western boreal
- [birdsNN_Global](https://github.com/tati-micheletti/birdsNN_Global) — neural network bird models (_Tati Micheletti_)
- [bird_modelLoad](https://github.com/FOR-CAST/bird_modelLoad) (_Sourav Das_)
- [bird_modelPredict](https://github.com/FOR-CAST/bird_modelPredict) (_Sourav Das_)
- [bird_covariateTranslator](https://github.com/FOR-CAST/bird_covariateTranslator) — turns LandR vegetation output into SCANFI-style covariates for bird models (_Sourav Das_)
- [LandbirdNRV_Prepinput](https://github.com/FOR-CAST/LandbirdNRV_Prepinput) (_Sourav Das_)
- [fitBirdBiomassModel](https://github.com/tati-micheletti/fitBirdBiomassModel) — songbird response to harvesting, from eBird and kNN biomass (_Tati Micheletti_)
- [waterfowl](https://github.com/tati-micheletti/waterfowl) — ensemble waterfowl forecasts (_Tati Micheletti_)
- [posthocBirdsNWT](https://github.com/tati-micheletti/posthocBirdsNWT) (_Tati Micheletti_)
- [bootRasterCombine](https://github.com/PredictiveEcology/bootRasterCombine) — averages and mosaics bootstrapped BAM density maps (_Alex Chubaty, Isolde Lane-Shaw_)
- [postHocBinning](https://github.com/PredictiveEcology/postHocBinning) — bird density by cover and age class (_Alex Chubaty, Isolde Lane-Shaw_)
- [mapBins](https://github.com/PredictiveEcology/mapBins) (_Isolde Lane-Shaw_)

---

## Other wildlife and biodiversity

- [wolfAlps](https://github.com/PredictiveEcology/wolfAlps) — wolf demography and dispersal in the Italian Alps (_Sarah Bauduin, Eliot McIntire_)
- [priorityPlaces](https://github.com/PredictiveEcology/priorityPlaces) — conservation prioritisation (_Tati Micheletti, Alex Chubaty_)
- [priorityPlaces_DataPrep](https://github.com/PredictiveEcology/priorityPlaces_DataPrep) (_Tati Micheletti, Alex Chubaty_)
- [comm_metricsNWT](https://github.com/tati-micheletti/comm_metricsNWT) — community metrics (_Ana Raymundo, Steve Cumming_)

---

## Human disturbance and land use

_Tati Micheletti, Alex Chubaty_

- [anthroDisturbance_DataPrep](https://github.com/tati-micheletti/anthroDisturbance_DataPrep) — harmonises many disturbance datasets into one structure
- [anthroDisturbance_Generator](https://github.com/tati-micheletti/anthroDisturbance_Generator) — simulates future disturbance
- [potentialResourcesNT_DataPrep](https://github.com/tati-micheletti/potentialResourcesNT_DataPrep) — mining and oil/gas potential, Northwest Territories
- [potentialResourcesYT_DataPrep](https://github.com/FOR-CAST/potentialResourcesYT_DataPrep) — the same, for Yukon
- [disturbanceGenerator_NT](https://github.com/tati-micheletti/disturbanceGenerator_NT)
- [lineDensity](https://github.com/tati-micheletti/lineDensity) — linear feature density (_Mario van Telgen_)
- [focalStatsCalculation](https://github.com/tati-micheletti/focalStatsCalculation) — focal statistics on large rasters
- [attributionCovars](https://github.com/tati-micheletti/attributionCovars) — builds covariates for attribution analyses

Project example: [anthropogenicDisturbance_Demo](https://github.com/tati-micheletti/anthropogenicDisturbance_Demo)

---

## Climate and weather data

- [canClimateData](https://github.com/PredictiveEcology/canClimateData) — historic and projected Canadian climate, ready for LandR and fireSense (_Ian Eddy, Alex Chubaty, Eliot McIntire_)
- [climateYear](https://github.com/PredictiveEcology/climateYear) — picks the climate year to use in a run (_Ian Eddy_)
- [fireWeather](https://github.com/CeresBarros/fireWeather) (_Ceres Barros_)
- [canWind](https://github.com/achubaty/canWind) (_Alex Chubaty, Eliot McIntire_)

---

## Study areas and project setup

Small modules that define where a simulation runs and assemble its starting data.

- [WBI_dataPrep_studyArea](https://github.com/PredictiveEcology/WBI_dataPrep_studyArea) — western boreal Canada (_Ian Eddy, Alex Chubaty, Eliot McIntire_)
- [Ontario_preamble](https://github.com/FOR-CAST/Ontario_preamble) — Ontario AOU and Ring of Fire (_Alex Chubaty, Ian Eddy_)
- [Quebec_fires_preamble](https://github.com/ianmseddy/Quebec_fires_preamble) (_Ian Eddy, Alex Chubaty_)
- [RIAlandscapes_studyArea](https://github.com/ianmseddy/RIAlandscapes_studyArea) (_Ian Eddy_)
- [LandR_MPB_studyArea](https://github.com/achubaty/LandR_MPB_studyArea) (_Alex Chubaty_)
- [caribouNT_studyArea](https://github.com/PredictiveEcology/caribouNT_studyArea)
- [getReadySimulationFiles](https://github.com/tati-micheletti/getReadySimulationFiles) — pulls prepared files from Google Drive (_Tati Micheletti_)

---

## Summaries, validation and reporting

- [NRV_summary](https://github.com/FOR-CAST/NRV_summary) — natural range of variation post-processing and reports (_Alex Chubaty_)
- [Biomass_summary](https://github.com/PredictiveEcology/Biomass_summary) (_Alex Chubaty, Tati Micheletti, Ian Eddy_)
- [fireSense_summary](https://github.com/PredictiveEcology/fireSense_summary) (_Alex Chubaty, Tati Micheletti, Ian Eddy_)
- [burnSummaries](https://github.com/PredictiveEcology/burnSummaries) (_Alex Chubaty_)
- [Biomass_validationKNN](https://github.com/PredictiveEcology/Biomass_validationKNN) (_Ceres Barros, Eliot McIntire_)
- [BiomeBGC_validationFluxTower](https://github.com/PredictiveEcology/BiomeBGC_validationFluxTower) (_Dominique Caron, Céline Boisvenue_)
- [posthocLandR](https://github.com/tati-micheletti/posthocLandR) (_Tati Micheletti_)

### Simulation monitoring

_Tati Micheletti, Lisa Hildebrand_

- [inputs_Monitor](https://github.com/tati-micheletti/inputs_Monitor)
- [dataPrep_Monitor](https://github.com/tati-micheletti/dataPrep_Monitor)
- [models_Monitor](https://github.com/tati-micheletti/models_Monitor)

---

## Teaching examples

Small modules written to show how SpaDES works. Good places to start reading code.

_Tati Micheletti_

- [speciesAbundance](https://github.com/tati-micheletti/speciesAbundance) — example 1 of 3
- [temperature](https://github.com/tati-micheletti/temperature) — example 2 of 3
- [speciesAbundTempLM](https://github.com/tati-micheletti/speciesAbundTempLM) — example 3 of 3
- [evaluateLM](https://github.com/tati-micheletti/evaluateLM) — how to evaluate a fitted model
- [smurfsMovement](https://github.com/tati-micheletti/smurfsMovement) — a movement model, deliberately silly

Also worth a look: [SpaDES4Dummies](https://github.com/CeresBarros/SpaDES4Dummies) (_Ceres Barros_),
the demonstration modules in
[this repository's `modules/` folder](https://github.com/PredictiveEcology/SpaDES-modules/tree/master/modules)
(`gameOfLife`, `LCC2005`, and others), and the worked examples in the
[book](https://predictiveecology.org/training/_book/).

---

## Retired

Superseded by the modules named beside them. **Note the ·N counts**: several are
archived yet still called by live projects, so they cannot simply be deleted — the
callers have to migrate first.

- [LandWeb_summary](https://github.com/PredictiveEcology/LandWeb_summary) → [NRV_summary](https://github.com/FOR-CAST/NRV_summary)
- [LandWeb_output](https://github.com/PredictiveEcology/LandWeb_output) → [NRV_summary](https://github.com/FOR-CAST/NRV_summary)
- [timeSinceFire](https://github.com/PredictiveEcology/timeSinceFire) → [burnSummaries](https://github.com/PredictiveEcology/burnSummaries)
- [LandWeb_preamble](https://github.com/PredictiveEcology/LandWeb_preamble)
- [fireSense_SizeFit](https://github.com/PredictiveEcology/fireSense_SizeFit)
- [fireSense_SizePredict](https://github.com/PredictiveEcology/fireSense_SizePredict)
- [scfmModules](https://github.com/PredictiveEcology/scfmModules) — the pre-2019 SCFM collection → [scfm](https://github.com/PredictiveEcology/scfm)

---

## Modules elsewhere

Modules maintained outside the accounts surveyed here:

- [ws3](https://github.com/gparadis/ws3) and [spades_ws3](https://github.com/UBC-FRESH/spades_ws3) — wood supply simulation (_Greg Paradis_)
- [timeSinceFire](https://github.com/fRI-Research/timeSinceFire), [LandWeb_preamble](https://github.com/fRI-Research/LandWeb_preamble), [LandWeb_output](https://github.com/fRI-Research/LandWeb_output) — fRI Research
- TriSect — spruce budworm modules (_Louis-Etienne Robert, Barry Cooke_); the repositories listed on earlier versions of this page are no longer public
