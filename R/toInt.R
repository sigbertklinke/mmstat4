#' toInt
#'
#' Converts \code{x} to an integer.
#' If the conversion fails or the integer is outside \code{min} and \code{max} then \code{NA_integer_} is returned
#'
#' @param x input object
#' @param min numeric: minimal value
#' @param max numeric: maximal value
#'
#' @return a single integer value
#' @export
#'
#' @examples
#' toInt(3.0)
#' toInt("3.0")
#' toInt("test")
toInt <- function(x, min=-Inf, max=+Inf) {
  x <- suppressWarnings(as.integer(x))
  if (length(x)!=1) return(NA_integer_)
  if(is.na(x)) return(NA_integer_)
  if (x<min) return(NA_integer_)
  if (x>max) return(NA_integer_)
  x
}
