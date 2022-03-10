#' prg
#'
#' Returns a list of matching names of an example file or app in \code{mmstat4}. Note that \code{file}
#' is interpreted as a regular expression.
#'
#' @param file character: a file name (default: \code{NULL})
#' @param pattern character: regular expression to be matched in the file names (default: \code{NULL})
#' @param n integer: number of similar files shown if no \code{file} can be found (default: \code{5})
#' @param ... further parameters given to \code{grepl}
#'
#' @return the complete path including the file name(s)
#' @importFrom utils adist
#' @importFrom stats na.omit
#' @export
#'
#' @examples
#' prg() # all available example files
#' prg("mmstat/lottozahlen.R")
#' if(interactive()) {
#'   edit(prg("mmstat/lottozahlen.R"))       # open text editor
#'   file.edit(prg("mmstat/lottozahlen.R"))  # open in RStudio
#' }
prg <- function(file=NULL, pattern=NULL, n=5, ...) {
  normPath <- function(file) {
    if (.Platform$file.sep=="/") {
      sep <- strsplit(file, .Platform$file.sep, fixed=TRUE)
    } else {
      sep <- strsplit(file, sprintf("[/|%s]{1}", .Platform$file.sep), fixed=FALSE)
    }
    sapply(sep,
           function(p) {
             dontkeep <- rep(FALSE, length(p))
             if (any(p=="sigbertklinke")) {
               pos <- which(p=="examples")
               if (length(pos)) dontkeep[1:pos] <- TRUE
             }
             dontkeep <- dontkeep | (p==".") | (p=="")
             do.call("file.path", as.list(p[!dontkeep]))
           })
  }
  #
  path  <- system.file("examples", package = "mmstat4")
  inst  <- list.files(path=path, recursive = TRUE)
  if (!is.null(pattern)) {
    args <- list(...)
    args$pattern <- pattern
    args$x <- inst
    inst <- inst[do.call("grepl", args)]
  }
  if (is.null(file)) return(inst)
  file <- normPath(file)
  ret  <- lapply(file, function(f) {
    # simple file
    if (f %in% inst) return(paste0(path, "/", f))
    # check for app.R
    app <- paste0(f, "/app.R")
    if (app %in% inst) return(paste0(path, "/", app))
    # error
    index  <- na.omit(order(adist(basename(inst), basename(f)))[1:n])
    errmsg <- c(sprintf("Sorry, file \"%s\" does not exist :( Maybe you meant:\n", f),
                sprintf("    %s\n", inst[index]))
    stop(errmsg)
  })
  unlist(ret)
}
