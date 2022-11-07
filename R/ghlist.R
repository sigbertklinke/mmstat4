#' ghlist
#'
#' Returns unique (short) names to access each file in the repository.
#'
#' @inheritParams base::grepl
#'
#' @return character vector of short names
#' @export
#'
#' @examples
#' if (interactive()) ghlist()
ghlist <- function(pattern='.', ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE) {
  ghget(getOption("mmstat.repo"))
  files <- strsplit(mmstat$files, '/', fixed=TRUE)
  cmax  <- max(lengths(files))
  nfiles <- length(files)
  files <- lapply(files, function(e) { v <- rep(NA_character_, cmax); v[1:length(e)] <- rev(e); v})
  m     <- matrix(unlist(files), nrow=length(files), ncol=cmax, byrow=TRUE)
  for (i in 1:(cmax-1)) {
    dups <- duplicated(m[,1:i])|duplicated(m[,1:i], fromLast=TRUE)
    m[!dups, (i+1):cmax] <- NA_character_
  }
  x <- apply(m, 1, function(e) { e <- rev(e); paste0(e[!is.na(e)], collapse="/") })
  x[grepl(pattern, x, ignore.case, perl, fixed, useBytes)]
}
