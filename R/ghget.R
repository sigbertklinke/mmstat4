#' ghget
#'
#' Makes a repository the active repository and downloads it if necessary.
#' If the function is run interactively then you are asked if you want to store the repository in the application directory
#' [rappdirs::user_data_dir()] for `mmstat4` or in the temporary directory [tempdir()]. Otherwise the stored directory
#' path is used (usually [tempdir()]).
#'
#' Note, the list of repository names, directories and urls is stored in the installation directory, too.
#'
#' @param ... parameters to set and activate a repository
#' @param .force logical: download and unzip in any case? (default: `FALSE`)
#'
#' @return the name of the activated directory
#' @importFrom utils download.file unzip askYesNo
#' @importFrom rappdirs user_data_dir
#' @export
#'
#' @examples
#' if (interactive()) {
#'
#' }
ghget <- function(..., .force=FALSE) {
  # analyse function parameter
  call <- match.call()
  key  <- NULL
  if (length(call)==1) {
    key <- 'hu.data'
  } else {
    ncall <- names(call)
    if (is.null(ncall)) ncall <- rep('', length(call))
    ccall <- sapply(call, class)
    for (i in 2:length(call)) {
      if (ncall[i]=='') { # no assignment
        key <- if (ccall[i]=="call") eval(call[[i]]) else as.character(call[[i]])
      } else {
        if (ncall[i]!='.force') {
          key <- ncall[i]
          mmstat$repository[[key]] <- list(url=if (ccall[[i]]=='call') eval(call[[i]]) else as.character(call[[i]]), dir='')
        }
      }
      if (is.null(mmstat$repository[[key]])) warning(sprintf("Repository '%s' does not exist", key))
    }
  }
  #
  stopifnot(key %in% names(mmstat$repository))
  # download zip file, if neccessary
  exdir <- mmstat$repository[[key]]$dir
  if (exdir=='') exdir <- tempdir()
  destfile <- paste0(exdir, '/', key, ".zip")
  if (.force || !file.exists(destfile)) {
    appdir <- user_data_dir('mmstat4')
    if (interactive() && askYesNo(sprintf("Install downloaded repository to '%s'?", appdir))) {
      mmstat$repository[[key]]$dir <- appdir
    } else {
      mmstat$repository[[key]]$dir <- ''
    }
    exdir <- mmstat$repository[[key]]$dir
    if (exdir=='') exdir <- tempdir()
    destfile <- paste0(exdir, '/', key, ".zip")
    download.file(mmstat$repository[[key]]$url, destfile, quiet = !interactive())
    # build names
    mmstat$repository[[key]]$files  <- unzip(destfile, exdir=exdir)
    mmstat$repository[[key]]$sfiles <- ghpath(ghdecompose(mmstat$repository[[key]]$files))
    # save modified repos, if necessary
    if (nchar(mmstat$repository[[key]]$dir)>0)
      saveRDS(mmstat$repository, file=paste0(appdir, "/repositories"), version=2)
  }
  mmstat$repo <- key
  key
}
