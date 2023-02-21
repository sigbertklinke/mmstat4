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
#' @param .tempdir logical or character: store download temporary or permanently (default: `getOption("mmstat4.tempdir")`)
#' * if `.tempdir==TRUE` then the downloaded zip file will be stored temporarily in [tempdir()]
#' * if `.tempdir==FALSE` then the downloaded zip file will be stored temporarily in [rappdirs::user_data_dir()]
#' * otherwise it is assumed that you give the name of an existing directory to store the downloaded zip file
#' @return invisibly the name of the current key
#' @importFrom utils download.file unzip
#' @importFrom rappdirs user_data_dir
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # get one of the default ZIP file from internet
#'   ghget("hu.data")
#'   # get a locally stored zip file
#'   ghget(dummy2=system.file("zip", "mmstat4.dummy.zip", package="mmstat4"))
#'   # get from an URL
#'   ghget(dummy.url="https://github.com/sigbertklinke/mmstat4.dummy/archive/refs/heads/main.zip")
#' }
ghget <- function(..., .force=FALSE, .tempdir=TRUE) {
  # analyse function parameter
  args <- list(...)
  stopifnot(length(args)==1)
  nargs <- names(args)
  if (is.null(nargs)) { # repo name (must exist)
    key <- if (is.null(args[[1]])) 'hu.data' else as.character(args[[1]])
    stopifnot(key %in% names(mmstat$repository))
  } else { # repo=URL or file
    key <- nargs
    # determine target directory after download
    if (is.logical(.tempdir)) {
      if (isFALSE(.tempdir)) {
        mmstat$repository[[key]]$dir <- user_data_dir('mmstat4')
      } else {
        mmstat$repository[[key]]$dir <- ''
      }
    } else {
      mmstat$repository[[key]]$dir <- as.character(.tempdir)
    }
    mmstat$repository[[key]]$url <- as.character(args[[1]])
  }
  exdir <- mmstat$repository[[key]]$dir
  if (exdir=='') exdir <- tempdir()
  # download zip file, if neccessary
  destfile <- paste0(exdir, '/', key, ".zip")
  if (.force || !file.exists(destfile)) {
    if (file.exists(mmstat$repository[[key]]$url)) {
      file.copy(mmstat$repository[[key]]$url, destfile)
    } else {
      res <- try(download.file(mmstat$repository[[key]]$url, destfile, quiet = !interactive()), silent = !interactive())
      if ("try-error" %in% class(res)) stop(sprintf("URL invalid or file not found: %s", args[[1]]))
    }
    # build names
    mmstat$repository[[key]]$files  <- normalizePath(unzip(destfile, exdir=exdir), winslash="/")
    mmstat$repository[[key]]$sfiles <- ghpath(ghdecompose(mmstat$repository[[key]]$files))
    # save modified repos, if necessary
    if (nchar(mmstat$repository[[key]]$dir)>0) saveRDS(mmstat$repository, file=paste0(exdir, "/repositories"), version=2)
  }
  mmstat$repo <- key
  invisible(key)
}
