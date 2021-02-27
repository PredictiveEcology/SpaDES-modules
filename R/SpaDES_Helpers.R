#' Simple function to download a SpaDES module as Github repository
#'
#' @param gitRepo A github repository, presented in the standard R way, with
#'   \code{account/repository@branch}
#' @param overwrite Logical. If \code{TRUE}, then the download will delete any
#'   existing folder with the same name as the \code{repository}
#'   provided in \code{gitRepo}
#' @param modulePath A local path in which to place the full module, within
#'   a subfolder ... i.e., the source code will be downloaded to here:
#'   \code{file.path(modulePath, repository)}
#' @export
getModule <- function(gitRepo, overwrite = FALSE, modulePath = ".") {
  if (!dir.exists(modulePath)) dir.create(modulePath, recursive = TRUE)
  gr <- splitGitRepo(gitRepo)
  ar <- file.path(gr$acct, gr$repo)
  repoFull <- file.path(modulePath, gr$repo)
  zipFileName <- normalizePath(paste0(repoFull, ".zip"), winslash = "/", mustWork = FALSE)
  for (i in 1:2) {
    url <- paste0("http://github.com/",ar,"/archive/",gr$br,".zip")
    out <- try(download.file(url, destfile = zipFileName))
    if (is(out, "try-error") && identical(gr$br, "master"))
      gr$br <- "main"
    else
      break
  }
  out <- unzip(zipFileName, exdir = modulePath) # unzip it
  if (dir.exists(repoFull))
    if (isTRUE(overwrite)) {
      unlink(repoFull, recursive = TRUE)
    } else {
      stop(repoFull, " directory already exists. Use overwrite = TRUE if you want to overwrite it")
    }
  badDirname <- unique(dirname(out))[1]
  file.rename(badDirname, gsub(basename(badDirname), gr$repo, badDirname)) # it was downloaded with a branch suffix
  unlink(zipFileName)
  message(gitRepo, " downloaded and placed in ", normalizePath(repoFull, winslash = "/"))
  possRmd <- normalizePath(winslash = "/", file.path(repoFull, paste0(gr$repo, ".Rmd")), mustWork = FALSE)
  if (file.exists(possRmd))
    message("To run it, try: \nfile.edit('", possRmd,"')")
  return(normalizePath(repoFull))
}

splitGitRepo <- function(gitRepo) {
  grSplit <- strsplit(gitRepo, "/|@")[[1]]
  acct <- grSplit[[1]]
  repo <- grSplit[[2]]
  if (length(grSplit) > 2) {
    br <- grSplit[[3]]
  } else {
    br <- "master"
  }
  list(acct = acct, repo = repo, br = br)
}

#' Install R Package from GitHub source code
#'
#' A lightweight alternative to \code{devtools::install_github}. All dependencies
#' must have been installed already for this to work.
#'
#' @param gitRepo A repository in the form: Account/Repository@Branch or Account/Repository@SHA
#' @param libPath The folder where you would like the package installed. Defaults
#'   to \code{.libPaths()[1]}
#' @export
installGithubPackage <- function(gitRepo, libPath = .libPaths()[1]) {
  gr <- splitGitRepo(gitRepo)
  modulePath <- file.path(tempdir(), paste0(sample(LETTERS, 8), collapse = ""))
  dir.create(modulePath, recursive = TRUE)
  out <- getModule(gitRepo, overwrite = TRUE, modulePath = modulePath)
  orig <- setwd(modulePath)
  if (nchar(Sys.which("R")) > 0) {
  out1 <- system(paste("R CMD build ", gr$repo), intern = TRUE)
  buildingLine <- grep("building", out1, value = TRUE)
  packageTarName <- strsplit(buildingLine, "'")[[1]][2]
  if (is.na(packageTarName)) { # linux didn't have that character
    packageTarName <- gsub(paste0("^.*(", gr$repo, ".*tar.gz).*$"), "\\1", buildingLine)
  }
  system(paste0("R CMD INSTALL --library=", normalizePath(libPath, winslash = "/"), " ",packageTarName), wait = TRUE)
  } else {
    message("Can't install packages this way because R is not on the search path")
  }
}

#' @rdname installGithubPackage
#' @export
installGitHubPackage <- installGithubPackage

#' Install SpaDES packages, making sure to update.packages first
#'
#' @param ask Passed to \code{update.packages}
#' @param type passed to both \code{update.packages} and \code{install.packages}. This
#'   will set \code{"binary"} on windows, if not set, to get the binary packages from CRAN
#' @param libPath Passed to both \code{update.packages(lib.lob = libPath)} and
#'   \code{install.packages(lib = libPath, ...)}
installSpaDES <- function(ask = FALSE, type, libPath = .libPaths()[1],
                          versions = c(SpaDES.core = "1.0.5", SpaDES.tools = "0.3.6"),
                          dontUpdate = c("scam")) {
  srch <- search()
  basePkgs <- dir(tail(.libPaths(),1))
  basePkgs <- c(basePkgs, "GlobalEnv", "Autoloads")
  nonBase <- lapply(basePkgs, function(bp) {
    srch <<- grep(bp, srch, value = TRUE, invert = TRUE)
  })

  if (length(srch) > 0) {
    message("It looks like you may need to restart your R session to get an R session without ",
            "R packages loaded already. If you are using RStudio and you are unable to restart without",
            "lots of R packages being pre-loaded, you may need to run this from a non-RStudio",
            " R session.")
    out <- readline("Do you want to proceed anyway? Y or N")
    if (!identical("y", tolower(out)))
        stop("Try to restart R with Ctrl-Alt-F10 if you are in RStudio")
  }
  writeable <- unlist(lapply(.libPaths(), file.access, mode = 2)) == 0
  args <- list(checkBuilt = TRUE, ask = ask)
  if (any(writeable)) {
    libPathsForUpdate <- .libPaths()[writeable]
    args$lib.loc <- libPathsForUpdate
  }
  isWin <- identical("windows", .Platform$OS.type)
  if (isWin && missing(type))
    args$type <- "binary"
  olds <- do.call(old.packages, args)
  toUpdate <- setdiff(olds[,"Package"], dontUpdate)
  args[["pkgs"]] <- toUpdate
  args[["dependencies"]] <- FALSE
  do.call(install.packages, args)

  #  install
  args <- list(c("SpaDES.core", "SpaDES.tools"), dependencies = TRUE)
   if (isWin && missing(type))
    args$type <- "binary"

  if (!isWin && !dir.exists(file.path(.libPaths()[1], "igraph")))
    install.packages("igraph", type = "source", lib = libPath, repos = "https://cran.rstudio.com") # igraph needs to be installed from source
  ip <- installed.packages()
  versions <- versions[args[[1]]]
  whichOK <- unlist(lapply(seq(versions), function(ind) {
    ok <- (!identical(as.character(packageVersion(names(versions)[ind])), versions[ind]))
    if (identical(ok, TRUE))
      message("skipping install of ", names(versions)[ind], "; version is OK")
    ok
  }))
  args[[1]] <- args[[1]][!whichOK]

  if (length(args[[1]])) {
    do.call(install.packages, args)
  }
  return(invisible())
}

#' Pre-test for packages in SpaDES modules
#'
#' This function can be put at the start of project code. It will
#' only be necessary if there are multiple \code{simInit} calls (i.e., the project
#' doesn't use only one call). It will check all modules in \code{modulePath} for
#' package dependencies. It will prompt
#' @param modulePath The path to modules, as per \code{SpaDES.core::setPaths}
#' @examples
#' \dontrun{
#' source("https://raw.githubusercontent.com/PredictiveEcology/SpaDES-modules/master/R/SpaDES_Helpers.R")
#' out <- makeSureAllPackagesInstalled(modulePath = "modules")
#' }
makeSureAllPackagesInstalled <- function(modulePath) {
  AllPackagesFile <- "._AllPackages.rds"
  if (!file.exists(AllPackagesFile)) {
    AllModules <- dir(modulePath)
    if (is(try(packageVersion("SpaDES.core"), silent = TRUE), "try-error")) {
      if (!require("Require")) {install.packages("Require"); require("Require")}
      message("Need to install SpaDES.core from CRAN to continue")
      Require("SpaDES.core")
    }
    AllPackages <- lapply(AllModules, function(mod) {
      print(mod)
      SpaDES.core::packages(modules = mod, paths = modulePath)
    })

    AllPackagesUnlisted <- unname(unlist(AllPackages))
    out <- Require::Require(require = FALSE, AllPackagesUnlisted, install = FALSE, verbose = TRUE)
    out <- attr(out, "Require")
    okVersions <- Require::getPkgVersions(out)
    okInstalled <- all(out$installed)
    okVersion <- all(okVersions$compareVersion >= 0)
    data.table::setorderv(out, "Package")
    data.table::setnames(out, old = "Version", "InstalledVersion")
    colsToShow <- c("packageFullName", "Package", "InstalledVersion", "correctVersion")
    out <- out[compareVersion <0, ..colsToShow]

    uniquedPkgs <- unique(out$Package)
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(length(uniquedPkgs)))

    if (!all(okVersion & okInstalled & anyLoaded)) {
      obj <- list(state = out, AllPackagesUnlisted = AllPackagesUnlisted)
      saveRDS(obj, file = AllPackagesFile)
      message("The following packages are in an incorrect state: ")
      reproducible::messageDF(print(out))
      stop("Restart R; Run this function again immediately.", call. = FALSE)
    }
  } else {
    AllPackagesUnlisted <- readRDS(AllPackagesFile)
    uniquedPkgs <- unique(AllPackagesUnlisted$state$Package)
    anyLoaded <- vapply(uniquedPkgs, function(pkg) isNamespaceLoaded(pkg), FUN.VALUE = logical(length(uniquedPkgs)))

    if (anyLoaded)
      stop("Some packages that need to be updated are still loaded; please restart R.",
           "You may have to change your settings so packages don't get automatically loaded")
    Require::Require(require = FALSE, AllPackagesUnlisted$AllPackagesUnlisted, upgrade = FALSE)
    unlink(AllPackagesFile)
  }
}

