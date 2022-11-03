#' ghopen
#'
#' Opens a R or Rmarkdown into the editor, starts a Shiny app or opens a data set
#' previously downloaded from a GitHub repository.
#'
#' @param x character(1): name of the file, app or data set
#' @param ... further parameters
#' @param quiet logical: should the download progess be shown (default: `!interactive()`)
#'
#' @return a data set, open a file in RStudio or runs a shiny app
#' @importFrom shiny runApp
#' @importFrom rstudioapi navigateToFile
#' @importFrom utils browseURL
#' @importFrom tools file_ext
#' @importFrom rio import
#' @export
#'
#' @examples
#' if (interactive()) x <- ghopen("bank2.SAV")
ghopen <- function(x, ..., quiet=!interactive()) {
  stopifnot(length(x)==1)
  ghget(quiet=quiet)
  # build URLs
  x    <- strsplit(x, '/', fixed=TRUE)[[1]]
  keep <- rep(TRUE, length(x))
  for (i in seq_along(x)) {
    if (x[i]=='.') keep[i] <- FALSE
    if (x[i]=='..') {
      keep[i] <- FALSE
      pos <- which(keep[1:i])
      if (length(pos)) keep[max(pos)] <- FALSE
    }
  }
  x <- paste0(x[keep], collapse="/")
  #
  j <- which(endsWith(mmstat$files, x))
  if (length(j)==0) { # might be an app
    j <- c(which(endsWith(mmstat$files, paste0(x, '/app.R'))),
           which(endsWith(mmstat$files, paste0(x, '/ui.R'))))
    if (length(j)==0) stop(sprintf("No file for '%s' at GitHub found", x))
    file <- mmstat$files[j]
  }
  if (length(j)==1) file <- mmstat$files[j]
  if (length(j)>1) stop(sprintf("Several files for '%s' at GitHub found", x))
  #
  ext <- tolower(file_ext(file))
  ret <- NULL
  if (ext %in% c('', 'r', 'rmd')) {
    navigateToFile(file)
    return(invisible(file))
  }
  if (ext %in% c('html', 'pdf')) {
    browseURL(file, ...)
    return(invisible(file))
  }
  import(file, ...)
}
