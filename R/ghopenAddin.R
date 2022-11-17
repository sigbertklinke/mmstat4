splitpath <- function (files) {
  sfiles <- strsplit(files, "[\\/]")
  path  <- character(0)
  npath <- min(lengths(sfiles))
  for (i in 1:npath) {
    pathi <- unique(sapply(sfiles, '[', i))
    if (length(pathi)>1) break
    path[i] <- pathi
  }
  data.frame(commonpath=rep(paste0(path, collapse="/"), length(files)),
             uniquepath=sapply(sfiles, i=i, function(v, i) { if (i<length(v)) paste0(v[i:(length(v)-1)], collapse="/") else '' }),
             filename=sapply(sfiles, function(v) { v[length(v)] }),
             source=files)
}

#' ghopenAddin
#'
#' A RStudio addin to open a file from the downloaded zip file.
#'
#' @return nothing
#' @importFrom rstudioapi navigateToFile selectFile
#' @export
#'
#' @examples
#' if (interactive()) ghopenAddin()
ghopenAddin <- function() {
  pathes <- splitpath(ghlist(full.names=TRUE))
  if (nrow(pathes)) {
    file <- selectFile(path=paste0(pathes$commonpath[1], collapse="/"))
    if (length(file)) navigateToFile(file)
  }
}

#' ghappAddin
#'
#' Runs a Shiny app from the downloaded zip file.
#'
#' @return nothing
#' @importFrom shiny runApp
#' @importFrom tcltk tk_select.list
#' @export
#'
#' @examples
#' if (interactive()) ghappAddin()
ghappAddin <- function() {
  dirs  <- splitpath(ghlist(full.names=TRUE, pattern="/(server|app)\\.R$"))
  if (nrow(dirs)) {
    index <- which(tk_select.list(dirs$uniquepath, title="Select Shiny app")==dirs$uniquepath)
    if (length(index)) runApp(appDir=dirname(dirs$source[index]), launch.browser=TRUE)
  }
}
