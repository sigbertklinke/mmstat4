#' ghfile
#'
#' Finds either a unique match in the list of files or throws an error with possible candidate files.
#'
#' @param x character: a single file name
#' @param silent logical: if no (unique) match is found, then `NULL` is returned, otherwise an error is thrown (default: `FALSE`, throw error)
#' @param n logical: if `x` can not be found how many best matches should be returned (default: `6`)
#' @param msg character: error message how to put the file name(s (default: `%s`)
#'
#' @return the full matching file
#' @importFrom stringdist stringdistmatrix
#' @export
#'
#' @examples
#' ghfile("data/BANK2.sav")
#' if (interactive()) ghfile("data/BANK2.SAV")  # throws an error
ghfile <- function(x, n=6, silent=FALSE, msg="%s") {
  ghget(mmstat$repo)
  #
  n <- as.integer(n)
  if (n<1) n <- 6
  #browser()
  # normalize path
  xin  <- x[1]
  x    <- normpathes(x[1])[[1]]
#  strsplit(x, '[\\/]')[[1]]
#  keep <- rep(TRUE, length(x))
#  for (i in seq_along(x)) {
#    if (x[i]=='.') keep[i] <- FALSE
#    if (x[i]=='..') {
#      keep[i] <- FALSE
#      pos <- which(keep[1:i])
#      if (length(pos)) keep[max(pos)] <- FALSE
#    }
#  }
  # perfect match in short or long files
  files  <- mmstat$repository[[mmstat$repo]]$files
  sfiles <- mmstat$repository[[mmstat$repo]]$sfiles
  xn <- paste0(x, collapse='/')
  j <- which(sfiles==xn)
  if (length(j)==1) return(files[j])
  j <- which(endsWith(files, xn))
  if (length(j)==1) return(files[j])
  # if not then find best match
  lfiles <- strsplit(files, '/', fixed=TRUE)
  d <- sapply(lfiles, xi=x, function(lf, xi) {
      sum(apply(stringdistmatrix(lf, xi, method="cosine"), 2, min, na.rm=TRUE))
  })
  match <- which(d==0)
  if (length(match)==1) return(files[match])
  if (silent) return(NULL)
  if (length(match)==0) {
    dind <- order(d)[1:min(n, length(d))]
    file <- sfiles[dind]
  }
  if (length(match)>1) {
    if (silent) return(NULL)
    file <- sfiles[match][1:min(n, length(match))]
  }
  cat("Best matches:", "\n ")
  cat(paste0(" ", sprintf(msg, file), "\n"))
  stop(sprintf("No (unique) file '%s' found, check matches!", xin))
#
#  x <- paste0(x[keep], collapse="/")
  # check if perfect match
#  mmstat_files <- mmstat$repository[[mmstat$repo]]$files
#  file <- NULL
#  j <- which(mmstat_files==x)
#  if (length(j)==1) file <- mmstat_files[j]
#  if (is.null(file)) { # check if perfect match in short names
#    j <- which(ghlist()==x)
#    if (length(j)==1) file <- mmstat_files[j]
#  }
#  if (is.null(file)) {
#    # look from the end
#    j <- which(endsWith(mmstat_files, x))
#    if (length(j)==0) { # might be an app
#      j <- c(which(endsWith(mmstat_files, paste0(x, '/app.R'))),
#             which(endsWith(mmstat_files, paste0(x, '/ui.R'))))
#      if (!dup && (length(j)==0)) {
#        bm <- ghquery(x)
#        cat("Best matches:", "\n ")
#        cat(paste0(" ", sprintf(msg, bm), "\n"))
#        stop(sprintf("No file '%s' found, check matches!", x))
#      }
#      file <- mmstat_files[j]
#    }
#    if (length(j)==1) file <- mmstat_files[j]
#    if (!dup && (length(j)>1)) {
#      cat("Possible matches:", "\n ")
#      lof <- ghlist()
#      cat(paste0(" ", sprintf(msg, lof[j]), "\n"))
##      cat (paste0(" ", lof[j], "\n"))
#      stop(sprintf("Several files for '%s' found, check matches!", x))
#    }
#  }
#  file
}
