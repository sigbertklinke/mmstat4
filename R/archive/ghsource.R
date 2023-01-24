#' ghsource
#'
#' Takes a file from the ZIP file and sources it.
#'
#' @param x character: file name
#' @param ... further parameter given to [base::source()]
#'
#' @return the result from [base::source()]
#' @export
#'
#' @examples
#' ghsource("univariate/example_ecdf.R")
ghsource <- function(x, ...) {
  stopifnot(length(x)==1)
  file <- ghfile(x)
  source(file, ...)
}
