#' ghpath
#'
#' Returns a path for files based on `ghdecompose`.
#'
#' @param df data frame: returned from `ghdecompose`
#' @param from character: either `inpath` (default), `outpath`, `minpath`, or `filename`
#'
#' @return a character vector with file pathes
#' @export
#'
#' @examples
#' ghget("dummy")
#' pdf <- ghdecompose(ghlist(full.names=TRUE))
#' ghpath(pdf)
#' ghpath(pdf, 'o') # equals the input to ghdecompose
#' ghpath(pdf, 'i')
#' ghpath(pdf, 'm')
#' ghpath(pdf, 'f')
ghpath <- function(df, from=c("outpath", "inpath", "minpath", "filename")) {
  stopifnot('ghdecompose' %in% class(df))
  from <- match.arg(from)
  p <- apply(df[,which(names(df)==from):4,drop=FALSE], 1, function(r) { paste0(r, collapse="/") })
  p <- gsub("/+", "/", p)
  if (from!="outpath") {
    abspath <- startsWith(p, '/')
    p[abspath] <- substring(p[abspath], 2)
  }
  p
}
