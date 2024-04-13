#' ghget
#'
#' Makes a repository the active repository and downloads it if necessary.
#' The parameter `.tempdir` is `TRUE` (default) then the repository is stored in the
#' in the temporary directory [tempdir()] else in the application directory
#' [rappdirs::user_data_dir()] for `mmstat4`.
#' The parameter `.tempdir` is not `logical` then the value will be used as installation path.
#'
#' Note, the list of repository names, directories and urls is stored in the installation directory, too.
#'
#' @param ... parameters to set and activate a repository
#' @param .force logical: download and unzip in any case? (default: `FALSE`)
#' @param .tempdir logical or character: store download temporary or permanently (default: `getOption("mmstat4.tempdir")`)
#' @param .quiet logical: show repository read attempts (default: `!interactive()`)
#' * if `.tempdir==TRUE` then the downloaded zip file will be stored temporarily in [tempdir()]
#' * if `.tempdir==FALSE` then the downloaded zip file will be stored temporarily in [rappdirs::user_data_dir()]
#' * otherwise it is assumed that you give the name of an existing directory to store the downloaded zip file
#' @return the name of the current key or nothing if unsuccessful
#'
#' @importFrom utils download.file unzip URLencode tail
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom httr HEAD status_code
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
ghget <-  function(..., .force=FALSE, .tempdir=TRUE, .quiet=!interactive()) {
  args <- list(...)
  if (length(args)==0) args <- list(names(mmstat$repository)[1])
  stopifnot(length(args)==1)
  nargs <- names(args)
  if (is.null(nargs)) {
    key  <- normpathes(args[[1]])[[1]]
    file <- tail(key, 1)
    key  <- if (tools::file_ext(file) %in% c('zip')) tools::file_path_sans_ext(file) else file
  } else {
    key  <- nargs
  }
  # determine repos
  repos <- ghrepos(args[[1]])
#  if (is.null(nargs)) { # key or file
#    if (key %in% names(mmstat$repository)) {
#      repos <- mmstat$repository[[key]]$url
#    } else {
#      repos <- c(repos, paste0(sprintf("https://github.com/%s/archive/refs/heads/", args[[1]]), c("main.zip", "master.zip")))
#    }
#  }
  # check if one of the repos exist
  isfile <- NA
  for (i in 1:length(repos)) {
    if (file.exists(repos[i])) {
      isfile <- TRUE
      reposi <- repos[i]
      break
    }
    res <- try({
      response <- HEAD(repos[i])
      status_code(response) == 200
    }, silent = TRUE)
    if (!inherits(res, 'try-error') && res) {
      isfile <- FALSE
      reposi <- repos[i]
      break
    }
  }
  if (is.na(isfile)) {
    cat(paste0(repos, collapse="\n"), "\n")
    stop("None of the previously displayed possible ZIP files were found!")
  }
  # get download dir
  if (is.logical(.tempdir)) {
    ddir <- if (isFALSE(.tempdir)) user_data_dir('mmstat4') else tempdir()
  } else {
    ddir <- as.character(.tempdir)
  }
  #
  repop    <- normpathes(reposi)
  destfile <- paste0(gsub("[^[:alnum:]._]", "_", repop[[1]]), collapse="_")
  destfile <- paste(ddir, destfile, sep="/")
  rdown    <- 1
  if (!file.exists(destfile) || .force) {
    if (isfile) {
      if (!.quiet) cat("Read:", reposi, "\n")
      file.copy(repos[i], destfile, overwrite=TRUE)
    } else {
      if (!.quiet) cat("Download:", reposi, "\n")
      rdown <- try(download.file(repos[i], destfile, quiet = TRUE), silent = TRUE)
      if (inherits(rdown, 'try-error')) stop("Download failed!")
    }
  }
  # unzip, save key, repos, and build names
  mmstat$repository[[key]]$dir    <- ddir
  mmstat$repository[[key]]$url    <- reposi
  mmstat$repository[[key]]$venv   <- make.names(paste("mmstat4", key))
  mmstat$repository[[key]]$files  <- normalizePath(unzip(destfile, exdir=ddir), winslash="/")
  mmstat$repository[[key]]$sfiles <- ghpath(ghdecompose(mmstat$repository[[key]]$files), "minpath")
  saveRDS(mmstat$repository, file=paste0(ddir, "/repositories.rds"), version=2)
  if (rdown==0) mmstat$install <- c("R"=TRUE, "py"=TRUE)
  mmstat$repo <- key
  key
}
