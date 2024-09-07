#' openFile
#'
#' The function attempts to open a file either in RStudio or in a text editor, depending on the environment.
#' If the session is interactive, it tries to open the file in RStudio using [rstudioapi::navigateToFile()].
#' If RStudio is not available or the attempt fails, it opens the file in a text editor using [utils::edit()].
#' If the session is not interactive, it simply returns the contents of the file.
#'
#' @param file character: name of the file
#' @param ... further parameters give to [rstudioapi::navigateToFile()] or [utils::edit()]
#'
#' @return invisibly the result from `try(rstudioapi::navigateToFile(file))` or `try(utils::edit(file))`.
#' @importFrom rstudioapi isAvailable navigateToFile
#' @importFrom utils edit
#' @export
#'
#' @examples
#' openFile(system.file("rstudio", "addins.dcf", package = "mmstat4"))
openFile <- function(file, ...) {
  if (interactive()) {
    if (requireNamespace("rstudioapi", quietly = TRUE))  {
      res <- try(navigateToFile(file=file[1], ...), silent = TRUE)
      if (!inherits(res, "try-error")) return(invisible(res))
    }
    res <- try(edit(file=file[1], ...), silent = TRUE)
    if (!inherits(res, "try-error")) return(invisible(res))
  }
  readLines(file[1], warn=FALSE)
}
