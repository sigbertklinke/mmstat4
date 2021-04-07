#' prg
#'
#' Returns a list of matching names of an example file or app in \code{mmstat4}. Note that \code{file}
#' is interpreted as a regular expression.
#'
#' @param file character: a file name (default: \code{NULL})
#' @param n integer: number of similar files shown if no \code{file} can be found
#'
#' @return the complete path including the file name(s)
#' @importFrom stats na.omit
#' @importFrom utils adist
#' @importFrom shiny runApp
#' @export
#'
#' @examples
#' prg() # all available example files
#' prg("mmstat/lottozahlen.R")
#' if(interactive()) {
#'   edit(prg("mmstat/lottozahlen.R"))       # open text editor
#'   file.edit(prg("mmstat/lottozahlen.R"))  # open in RStudio
#' }
prg <- function(file=NULL, n=5) {
  inst  <- list.files(path=system.file("examples", package = "mmstat4"), recursive = TRUE)
  # clean for shiny apps
  shiny  <- grepl("/(app|ui|server).R$", inst)
  if (any(shiny)) {
    exdirs <- unique(dirname(inst[shiny]))
    keep   <- rep(TRUE, length(inst))
    for (i in 1:length(exdirs)) keep = keep & !startsWith(inst, exdirs[i])
    inst   <- c(inst[keep], paste0(exdirs, "/"))
  }
  files <- if (is.null(file)) inst else inst[grepl(file, inst)]
  if (length(files)>0) return(files)
  index  <- na.omit(order(adist(inst, file))[1:n])
  errmsg <- c(sprintf("Sorry, file \"%s\" does not exist :( Maybe you meant:\n", file),
              sprintf("    %s\n", inst[index]))
  stop(errmsg)
}
