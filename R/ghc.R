#' Creates a `ghdecompose` pbject
#'
#' `ghc` creates from a list of file names using [mmstat4::ghdecompose()] and deletes mssing files.
#'
#' @param ... list(s) of filenmaes
#'
#' @return a `ghdecompose` pbject
#' @export
#'
#' @examples
#' ghc(list.files(system.file(package="mmstat4"), recursive=TRUE))
ghc  <- function(...) {
  args  <- list(...)
  files <- normalizePath(unique(as.character(unlist(args))))
  fmiss <- !file.exists(files)
  if (any(fmiss)) warning(sprintf("Missing files:\n%s\n", paste0("  ", files[fmiss], collapse="\n")))
  ghdecompose(files)
}
