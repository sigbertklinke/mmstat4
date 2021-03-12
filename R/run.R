#' run
#'
#' \code{source}s the example file
#'
#' @param name character: name of example file
#' @param ... further parameters to \code{source}
#'
#' @return nothing
#' @export
#'
#' @examples
#' run("mmstat/lottozahlen.R")
run <- function(name, ...) {
  args <- list(...)
  args$file <- system.file("examples", name, package = "mmstat4")
  do.call(source, args)
}
