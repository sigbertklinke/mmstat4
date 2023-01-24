#' Rsolo
#'
#' Assume that all files given are valid R files which should run indepedently
#' of each other. If an error appears the execution is stopped.
#'
#' @param files character: file name(s)
#' @param start integer: numeric index from which file to start (default: `1`)
#' @param path character: path to start from (default: `getwd()`)
#'
#' @return nothing
#' @export
#'
#' @examples
#' if (interactive()) {
#'   files <- list.files(patter="*.R$", full.names=TRUE, recursive=TRUE)
#'   Rsolo(files)
#' }
Rsolo <- function(files, start=1, path=NULL) {
  index <- 1:length(files)
  if (start %in% index) index <- start:length(files)
  if (is.null(path)) path <- getwd()
  path <- normalizePath(path)
  setwd(path)
  for (i in index) {
    f <- files[i]
    print(sprintf("Rsolo %i: %s", i, f))
    setwd(dirname(f))
    stopifnot(system(sprintf("Rscript %s", basename(f)), wait = TRUE)==0)
    setwd(path)
  }
}
