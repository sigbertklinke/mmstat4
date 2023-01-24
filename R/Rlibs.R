#' Rlibs
#'
#' Counts the number of `library` and `require` commands in the files.
#'
#' @param files character: file name(s)
#'
#' @return a table how frequently the packages are called
#' @export
#'
#' @examples
#' if (interactive()) {
#'   files <- list.files(patter="*.R$", full.names=TRUE, recursive=TRUE)
#'   Rlibs(files)
#' }
Rlibs <- function(files) {
  index <- 1:length(files)
  fcont <- rep('', length(index))
  for (i in index) {
    f        <- files[i]
    fcont[i] <- readChar(f, file.info(f)$size)
  }
  m <- unlist(regmatches(fcont, gregexpr('(library|require)\\s*\\((.*?)\\)+', fcont)))
  m <- gsub("(library|require)", "", m)
  m <- gsub("['\"()]", "", m)
  table(sort(m))
}
