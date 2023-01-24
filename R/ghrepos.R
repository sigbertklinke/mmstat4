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
  ghget(mmstat$repo)
  data.frame(active=(names(mmstat$repository)==mmstat$repo),
             dir=sapply(mmstat$repository, '[[', 'dir'),
             url=sapply(mmstat$repository, '[[', 'url')
  )
}
