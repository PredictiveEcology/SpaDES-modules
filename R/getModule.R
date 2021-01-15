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
  # grSplit <- strsplit(gitRepo, "/|@")[[1]]
  # acct <- grSplit[[1]]
  # repo <- grSplit[[2]]
  # if (length(grSplit) > 2) {
  #   br <- grSplit[[3]]
  # } else {
  #   br <- "master"
  # }
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

buildPackage <- function(gitRepo, overwrite = FALSE, modulePath = ".") {
  gr <- splitGitRepo(gitRepo)
  capture.output(type = "message", {
    out <- getModule(gitRepo, overwrite, modulePath)
  })
  orig <- setwd(modulePath)
  out1 <- system(paste("R CMD build ", gr$repo), intern = TRUE)
  buildingLine <- grep("building", out1, value = TRUE)
  packageTarName <- strsplit(buildingLine, "'")[[1]][2]
  if (is.na(packageTarName)) { # linux didn't have that character
    packageTarName <- gsub(paste0("^.*(", gr$repo, ".*tar.gz).*$"), "\\1", buildingLine)
  }
  system(paste("R CMD INSTALL",packageTarName), intern = TRUE)
}
