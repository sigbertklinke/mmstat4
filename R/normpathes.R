#' normpathes
#'
#' Returns a list with normalized pathes.
#'
#' @param x file pathes
#'
#' @return A list of the same length as `x`, the i-th element of which contains the vector of
#' splits of `x[i]`.
#' @export
#'
#' @examples
#' normpathes("CRAN/../mmstat4/python/./ghdist.R")
normpathes <- function(x) {
  lapply(strsplit(x, '[\\/]'),
         function(v) {
           ind1 <- which(v=='.')
           ind2 <- which(v=='..')
           if (length(ind1)) v <- v[-ind1]
           if (length(ind2)) {
             for (i in seq_along(ind2)) {
               dind <- if (ind2[i]==1) ind2[i] else ind2[i]-c(1,0)
               v    <- v[-dind]
               ind2 <- ind2-length(dind)
             }
           }
           v
         })
}
