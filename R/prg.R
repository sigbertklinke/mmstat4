#' prg
#'
#' Returns the full name of an example file in \code{mmstat4} or a list of available example files.
#'
#' @param file character: file name (defauilt. \code{NULL})
#'
#' @return the complete path including the file name
#' @export
#'
#' @examples
#' prg() # all available example files
#' prg("mmstat/lottozahlen.R")
#' if(interactive()) {
#'   edit(prg("mmstat/lottozahlen.R"))        # open text editor
#'   file.edit (prg("mmstat/lottozahlen.R"))  # open in RStudio
#' }
prg <- function(file=NULL) {
  if (is.null(file)) return(list.files(path=system.file("examples", package = "mmstat4"), recursive = TRUE))
  system.file("examples", file, package = "mmstat4")
}
