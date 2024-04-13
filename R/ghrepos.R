#' ghrepos
#'
#' If `key` is `NULL`, then it returns the known repositories and where they are stored.
#' If `key` is not `NULL`, then possible addresses for a repository are returned .
#'
#' @param key character: "name" of the repository to find (default: `NULL`)
#'
#' @return a data frame with the data about the repositories
#' @export
#'
#' @examples
#' ghrepos()
ghrepos <- function(key=NULL) {
  if (is.null(key)) {
    ghget(mmstat$repo)
    return(data.frame(active=(names(mmstat$repository)==mmstat$repo),
                      dir=sapply(mmstat$repository, '[[', 'dir'),
                      url=sapply(mmstat$repository, '[[', 'url')))
  }
  repos <- NULL
  if (key %in% names(mmstat$repository)) repos <- c(repos, mmstat$repository[[key]]$url)
  repos <- c(repos, key)
  c(repos, paste0(sprintf("https://github.com/%s/archive/refs/heads/", key), c("main.zip", "master.zip")))
}
