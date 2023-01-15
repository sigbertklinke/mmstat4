#' ghload
#'
#' Loads a data file via [rio::import()] into R from a previously downloaded from a GitHub repository.
#'
#' @param x character(1): name of the file, app or data set
#' @param ... further parameters used in [rio::import()]
#'
#' @return a data set, open a file in RStudio or runs a shiny app
#' @importFrom rio import
#' @export
#'
#' @examples
#' if (interactive()) x <- ghload("bank2.SAV")
ghload <- function(x, ...) {
  stopifnot(length(x)==1)
  ghget(mmstat$repo)
  # normalize path
  x    <- strsplit(x, '[\\/]',)[[1]]
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
  # check if perfect match
  mmstat_files <- mmstat$repository[[mmstat$repo]]$files
  file <- NULL
  j <- which(mmstat_files==x)
  if (length(j)==1) file <- mmstat_files[j]
  if (is.null(file)) { # check if perfect match in short names
    j <- which(ghlist()==x)
    if (length(j)==1) file <- mmstat_files[j]
  }
  if (is.null(file)) {
    # look from the end
    j <- which(endsWith(mmstat_files, x))
    if (length(j)==0) { # might be an app
      j <- c(which(endsWith(mmstat_files, paste0(x, '/app.R'))),
             which(endsWith(mmstat_files, paste0(x, '/ui.R'))))
      if (length(j)==0) {
        bm <- ghquery(x)
        cat("Best matches:", "\n ")
        cat(paste0(" ", bm, "\n"))
        stop(sprintf("No file '%s' found, check matches!", x))
      }
      file <- mmstat_files[j]
    }
    if (length(j)==1) file <- mmstat_files[j]
    if (length(j)>1) {
      cat("Possible matches:", "\n ")
      lof <- ghlist()
      cat (paste0(" ", lof[j], "\n"))
      stop(sprintf("Several files for '%s' found, check matches!", x))
    }
  }
  import(file, ...)
}
