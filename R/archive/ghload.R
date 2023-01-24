#' ghload
#'
#' Loads a data file via [rio::import()] into R from a previously downloaded from a GitHub repository.
#'
#' @param x character(1): name of the file, app or data set
#' @param ... further parameters used in [rio::import()]
#'
#' @return a data set, open a file in RStudio or runs a shiny app
#' @importFrom rio import
#' @export
#'
#' @examples
#' if (interactive()) x <- ghload("bank2.SAV")
ghload <- function(x, ...) {
  stopifnot(length(x)==1)
  file <- ghfile(x)
  import(file, ...)
}
