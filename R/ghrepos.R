#' ghrepos
#'
#' Retuuns the known repositories and where there are stored. If `dir==''` then `tempdir()` is used for storage.
#'
#' @return a data frame with the data about the repositories
#' @export
#'
#' @examples
#' ghrepos()
ghrepos <- function() {
  data.frame(url=sapply(mmstat$repository, '[[', 'url'),
             dir=sapply(mmstat$repository, '[[', 'dir'))
}
