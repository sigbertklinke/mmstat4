#' ghget
#'
#' Downloads a GitHub repository to a temporary directory, if none given
#'
#' @param dir character: directory name (default: `tempdir()`)
#' @param url character: zip file name or repository, default is the mmstat4 repository
#' @param quiet logical: should the download progess be shown (default: `!interactive()`)
#'
#' @return nothing
#' @importFrom utils download.file unzip
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ghget()            # download to tempdir()
#'   ghget("mytempdir") # download to "mytempdir"
#' }
ghget <- function(dir=getOption("mmstat.dir", tempdir()),
                  url=getOption("mmstat.repo", "https://github.com/sigbertklinke/mmstat4/archive/refs/heads/main.zip"),
                  quiet=!interactive()) {
  if (is.null(mmstat$files)) {
    destfile <- paste0(dir, '/', basename(url))
    if (!file.exists(destfile)) download.file(url, destfile, quiet=quiet)
    mmstat$files <- unzip(destfile, exdir=dir)
  }
  if (dir!=tempdir()) options(mmstat.dir=dir)
}
