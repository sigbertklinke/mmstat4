#' Rdups
#'
#' Computes checksums to find duplicate files.
#'
#' @param files character: file name(s)
#' @param ... further parameters given to [digest::digest()]
#'
#' @return a list of file names with the same checksum or `NULL`
#' @importFrom digest digest
#' @export
#'
#' @examples
#' if (interactive()) {
#'   files <- list.files(pattern="*.R$", full.names=TRUE, recursive=TRUE)
#'   Rdups(files)
#' }
Rdups <- function(files, ...) {
  index <- 1:length(files)
  ret   <- list()
  for (i in index) {
    digi <- digest(file=files[i], ...)
    ret[[digi]] <- c(ret[[digi]], files[i])
  }
  nret <- lengths(ret)
  if (any(nret>1)) return(ret[nret>1])
  NULL
}
