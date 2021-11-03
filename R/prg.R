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
  normPath <- function(file) {
    if (.Platform$file.sep=="/") {
      sep <- strsplit(file, .Platform$file.sep, fixed=TRUE)
    } else {
      sep <- strsplit(file, sprintf("[/|%s]{1}", .Platform$file.sep), fixed=FALSE)
    }
    sapply(sep,
           function(p) {
             dontkeep <- (p==".") | (p=="")
             do.call("file.path", as.list(p[!dontkeep]))
           })
  }
  #
  path  <- system.file("examples", package = "mmstat4")
  inst  <- list.files(path=path, recursive = TRUE)
  if (is.null(file)) return(inst)
  file <- normPath(file)
  ret  <- lapply(file, function(f) {
    # check for shiny app and return path
    for (app in c("/app.R", "/ui.R", "/server.R")) {
      if (endsWith(f, app) && (f %in% inst)) return(paste0(dirname(f), "/"))
      if (paste0(f, app) %in% inst) return(paste0(f, "/"))
    }
    if (f %in% inst) return(f)
    index  <- na.omit(order(adist(inst, f))[1:n])
    errmsg <- c(sprintf("Sorry, file \"%s\" does not exist :( Maybe you meant:\n", f),
                sprintf("    %s\n", inst[index]))
    stop(errmsg)
  })
  unlist(ret)
}
