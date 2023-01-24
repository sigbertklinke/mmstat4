#' ghopen
#'
#' Opens a file in the RStudio editor or the local browser from a previously downloaded from a GitHub repository.
#' If the extension of the file is `html` or `pdf` then the file will be opened in the browser otherwise into
#' the RStudio editor.
#'
#' @param x character(1): name of the file, app or data set
#' @param ... further parameters used in [rstudioapi::navigateToFile()] or [utils::browseURL]
#'
#' @return a data set, open a file in RStudio or runs a shiny app
#' @importFrom rstudioapi navigateToFile
#' @importFrom utils browseURL adist
#' @importFrom tools file_ext
#' @importFrom rio import
#' @export
#'
#' @examples
#' if (interactive()) x <- ghopen("bank2.SAV")
ghopen <- function(x, ...) {
  stopifnot(length(x)==1)
  file <- ghfile(x)
  # open depending on extensions
  ext <- tolower(file_ext(file))
  if (ext %in% getOption("mmstat.ext.doc", c('html', 'pdf'))) {
    browseURL(file, ...)
    return(invisible(file))
  } else {
    navigateToFile(file)
  }
  return(invisible(file))
}
