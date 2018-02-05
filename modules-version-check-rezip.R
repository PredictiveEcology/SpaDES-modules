library(igraph)
library(SpaDES)

setwd("~/Documents/GitHub/SpaDES-modules")

# check all module versions, and rezip if needed
omit <- ".Rproj.user"

allModules <- list.dirs("modules/", recursive = FALSE) %>% basename()
currentModules <- if(length(omit)) {
  allModules[!allModules %in% omit]
} else {
  allModules
}
rezipped <- list()

sim <- simInit() ## workaround: need dummy simList to get metadata below

## overwrite existing zip?
overwrite = TRUE

out <- lapply(currentModules, function(x) {
  version <- SpaDES.core::moduleVersion(module = x, path = "modules") %>% as.character()
  zipFile <- paste0("modules/", x, "/", x, "_", version, ".zip")
  if(!file.exists(zipFile) | overwrite == TRUE) {
    SpaDES.core::zipModule(x, "modules")
    rezipped <<- append(rezipped, x)
  }
})
rezipped <- unlist(rezipped)

## status messages
m <- list(omit = omit, checked = currentModules, rezipped = rezipped)
out <- lapply(names(m), function(x) {
  m[[x]] <<- if ( is.null(m[[x]]) | length(m[[x]]) == 0 ) {
      "none"
  } else {
      m[[x]]
  }
})

message("Modules omitted:\n\t", paste(m$omit, collapse = "\n\t"))
message("Modules checked:\n\t", paste(m$checked, collapse = "\n\t"))
message("Modules rezipped:\n\t", paste(m$rezipped, collapse = "\n\t"))
