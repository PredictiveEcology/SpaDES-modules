getModule <- function(gitRepo, overwrite = FALSE, modulePath = ".") {
  grSplit <- strsplit(gitRepo, "/|@")[[1]]
  if (!dir.exists(modulePath)) dir.create(modulePath, recursive = TRUE)
  acct <- grSplit[[1]]
  repo <- grSplit[[2]]
  ar <- file.path(acct, repo)
  if (length(grSplit) > 2) {
    br <- grSplit[[3]]
  } else {
    br <- "master"
  }
  repoFull <- file.path(modulePath, repo)
  zipFileName <- paste0(repoFull, ".zip")
  for (i in 1:2) {
    url <- paste0("http://github.com/",ar,"/archive/",br,".zip")
    out <- try(download.file(url, destfile = zipFileName))
    if (is(out, "try-error") && identical(br, "master"))
      br <- "main"
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
  file.rename(badDirname, gsub(basename(badDirname), repo, badDirname)) # it was downloaded with a branch suffix
  unlink(zipFileName)
  message(gitRepo, " downloaded and placed in ", normalizePath(repoFull))
  message("To run it, try: \nfile.edit('",normalizePath(winslash = "/", file.path(repoFull, paste0(repo, ".Rmd"))),"')")
  return(normalizePath(repoFull))
}
