#' isLocal
#'
#' Checks if a Shiny app runs locally or on a server
#'
#' @return logical
#' @export
#'
#' @examples
#' isLocal()
isLocal <- function() { Sys.getenv('SHINY_PORT') == "" }
