#' gquery
#'
#' Queries the unique (short) names for each file in the repository.
#'
#' @param query character: query string
#' @param n integer: maximal number of matches to return
#' @param full.names logical: should full names used instead of short names (default: `FALSE`)
#'
#' @return character vector of short names fitting best to the query
#' @export
#'
#' @examples
#' if (interactive()) ghquery("bank")
ghquery <- function(query, n=6, full.names=FALSE) {
  overlap <- function(x, files) {
    x <- unique(tolower(strsplit(x, "")[[1]]))
    f <- strsplit(files, "")
    sapply(f, x=x, function(v, x) {
      v <- unique(tolower(v))
      1-length(intersect(v,x))/min(length(v),length(x))
    })
  }
  #
  stopifnot(length(query)==1)
  lof <- ghlist(full.names = full.names)
  dxl <- overlap(query, lof)
  o   <- order(dxl, nchar(lof))
  if (length(dxl)<=n) n <- length(dxl)
  lof[o[1:n]]
}
