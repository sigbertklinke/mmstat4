#' run
#'
#' \code{source}s the example file. The parameter \code{echo} from \code{source} is set to \code{TRUE} by default.
#'
#' @param name character: name of example file
#' @param ... further parameters to [base::source()]
#'
#' @return nothing
#' @export
#'
#' @examples
#' run("mmstat/lottozahlen.R")
#' run("mmstat/lottozahlen.R", echo=FALSE)
run <- function(name, ...) {
  args <- list(...)
  if (is.null(args$echo)) args$echo <- TRUE
  args$file <- system.file("examples", name, package = "mmstat4")
  do.call(source, args)
}
