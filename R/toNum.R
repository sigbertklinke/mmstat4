#' toNum
#'
#' Converts \code{x} to a numeric.
#' If the conversion fails or the value is outside \code{min} and \code{max} then \code{NA} is returned
#'
#' @param x input object
#' @param min numeric: minimal value
#' @param max numeric: maximal value
#'
#' @return a single integer value
#' @export
#'
#' @examples
#' toNum(3.0)
#' toNum("3.0")
#' toNum("test")
toNum <- function(x, min=-Inf, max=+Inf) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x)!=1) return(NA)
  if(is.na(x)) return(NA)
  if (x<min) return(NA)
  if (x>max) return(NA)
  x
}
