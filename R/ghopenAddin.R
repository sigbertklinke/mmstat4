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
  if(require("mmstat4")) {
    pathes <- ghdecompose(ghlist(full.names=TRUE))
    if (nrow(pathes)) {
      file <- selectFile(path=paste0(pathes$commonpath[1], collapse="/"))
      if (length(file)) navigateToFile(file)
    }
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
  if(require("mmstat4")) {
    dirs  <- ghdecompose(ghlist(full.names=TRUE, pattern="/(server|app)\\.R$"))
    if (nrow(dirs)) {
      index <- which(tk_select.list(dirs$minpath, title="Select Shiny app")==dirs$minpath)
      if (length(index)) runApp(appDir=dirname(dirs$source[index]), launch.browser=TRUE)
    }
  }
}
