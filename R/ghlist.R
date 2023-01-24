#' ghlist
#'
#' Returns unique (short) names for accessing each file in the repository according to a regular expression.
#' For details about regular expressions, see [base::regex].
#'
#' @inheritParams base::grepl
#' @param full.names logical: should full names returned instead of short names (default: `FALSE`)
#'
#' @return character vector of short names
#' @export
#'
#' @examples
#' if (interactive()) ghlist()
ghlist <- function(pattern='.', ignore.case = FALSE, perl = FALSE,
                   fixed = FALSE, useBytes = FALSE, full.names=FALSE) {
  ghget(mmstat$repo)
  x <- if (full.names) mmstat$repository[[mmstat$repo]]$files else  mmstat$repository[[mmstat$repo]]$sfiles
  setdiff(unique(x[grepl(pattern, x, ignore.case, perl, fixed, useBytes)]), '.')
}
