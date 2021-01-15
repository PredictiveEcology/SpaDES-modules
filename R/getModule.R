getModule <- function(gitRepo, overwrite = FALSE, modulePath = ".") {
  grSplit <- strsplit(gitRepo, "/|@")[[1]]
  acct <- grSplit[[1]]
  repo <- grSplit[[2]]
  ar <- file.path(acct, repo)
  if (length(grSplit) > 2) {
    br <- grSplit[[3]]
  } else {
    br <- "master"
  }
  for (i in 1:2) {
    url <- paste0("http://github.com/",ar,"/archive/",br,".zip")
    zipFileName <- paste0(repo, ".zip")
    out <- try(download.file(url, destfile = zipFileName))
    if (is(out, "try-error") && identical(br, "master"))
      br <- "main"
    else
      break
  }
  out <- unzip(zipFileName, exdir = modulePath) # unzip it
  if (dir.exists(repo))
    if (isTRUE(overwrite)) {
      unlink(repo, recursive = TRUE)
    } else {
      stop(repo, " directory already exists. Use overwrite = TRUE if you want to overwrite it")
    }
  badDirname <- unique(dirname(out))[1]
  file.rename(badDirname, gsub(basename(badDirname), repo, badDirname)) # it was downloaded with a branch suffix
  message(gitRepo, " downloaded and unzipped in ", normalizePath(file.path(modulePath, repo)))
  return(invisible())
}
