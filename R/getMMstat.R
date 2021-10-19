
#' getMMstat
#'
#' Allows to access the package internal \code{mmstat} environment.
#'
#' @param ... elements
#'
#' @return the choosen element
#' @export
#'
#' @examples
#' getMMstat('version')
getMMstat <- function (...) {
  ret  <- mmstat
  args <- list(...)
  for (i in seq(args)) ret <- ret[[args[[i]]]]
  ret
}
