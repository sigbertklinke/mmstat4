#' @rdname ghget
#' @aliases ghset
#' @title ghget
#' @description
#'
#' * `ghset` set for a key the installation directory and GitHubs Zip URL. If `dir` is `NULL` then `tempdir()` is used.
#' * `ghget` downloads the GitHub repository and unzips it if necessary.
#'
#' @param key character: directory name (default: `mmstat4`)
#' @param url character: URL to zip file of repository, default is the mmstat4 repository
#' @param install logical: call `ghget` in `ghset`? (default: `TRUE`)
#' @param force logical: download and unzip in any case? (default: `FALSE`)
#'
#' @return nothing
#' @importFrom utils download.file unzip
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ghget("dummy")
#'   ghget()        # or ghget("mmstat4")
#' }
ghget <- function(key="mmstat4", force=FALSE) {
  stopifnot(key %in% names(mmstat$repository))
  exdir <- mmstat$repository[[key]]$dir
  if (exdir=='') exdir <- tempdir()
  destfile <- paste0(exdir, '/', key, ".zip")
  if (!file.exists(destfile) || force) download.file(mmstat$repository[[key]]$url, destfile)
  mmstat$files <- unzip(destfile, exdir=exdir)
  # build short names
  files <- strsplit(mmstat$files, '/', fixed=TRUE)
  cmax  <- max(lengths(files))
  nfiles <- length(files)
  files <- lapply(files, function(e) { v <- rep(NA_character_, cmax); v[1:length(e)] <- rev(e); v})
  m     <- matrix(unlist(files), nrow=length(files), ncol=cmax, byrow=TRUE)
  for (i in 1:(cmax-1)) {
    dups <- duplicated(m[,1:i])|duplicated(m[,1:i], fromLast=TRUE)
    m[!dups, (i+1):cmax] <- NA_character_
  }
  mmstat$sfiles <- apply(m, 1, function(e) { e <- rev(e); paste0(e[!is.na(e)], collapse="/") })
  options(mmstat.repo=key)
}

#' @rdname ghget
#' @importFrom utils askYesNo
#' @importFrom rappdirs user_data_dir
#' @export
ghset <- function(key, url, install=TRUE) {
  if (grepl("[\\/]", key)) stop("Slashes or backslashes in a key are not allowed.")
  dir    <- ''
  appdir <- user_data_dir('mmstat4')
  if (interactive() && askYesNo(sprintf("Download and install repository to '%s'?", appdir))) dir <- appdir
  if (nchar(dir) && !dir.exists(dir)) dir.create(dir, recursive = TRUE)
  mmstat$repository[[key]] <- list(url=url, dir=dir)
  if (install) ghget(key)
  # build list
  if (nchar(dir)>0) saveRDS(mmstat$repository, file=paste0(appdir, "/repositories"), version=2)
  invisible(ghrepos())
}
