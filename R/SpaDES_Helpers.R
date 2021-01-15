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
#' @param libPath Passed to \code{install.packages(lib = libPath, ...)}
installSpaDES <- function(ask = FALSE, type, libPath = .libPaths()[1]) {
  srch <- search()
  basePkgs <- dir(tail(.libPaths(),1))
  basePkgs <- c(basePkgs, "GlobalEnv", "Autoloads")
  nonBase <- lapply(basePkgs, function(bp) {
    srch <<- grep(bp, srch, value = TRUE, invert = TRUE)
  })

  if (length(srch) > 0) {
    message("It looks like you may need to restart your R session to get an R session without",
            "R packages loaded already. If you are using RStudio and you are unable to restart without",
            "lots of R packages being pre-loaded, you may need to run this from a non-RStudio",
            " R session.")
    out <- readline("Do you want to proceed anyway? Y or N")
    if (!identical("y", tolower(out)))
        stop("Try to restart R with Ctrl-Alt-F10 if you are in RStudio")
  }
  args <- list(checkBuilt = TRUE, ask = ask)
  isWin <- identical("windows", .Platform$OS.type)
  if (isWin && missing(type))
    args$type <- "binary"
  do.call(update.packages, args)

  #  install
  args <- list(c("SpaDES.core", "SpaDES.tools"), dependencies = TRUE)
   if (isWin && missing(type))
    args$type <- "binary"
  if (!isWin && !require(igraph))
    install.packages("igraph", type = "source", lib = libPath) # igraph needs to be installed from source
  do.call(install.packages, args)
}
