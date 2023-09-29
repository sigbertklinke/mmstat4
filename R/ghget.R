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
#' @param .quiet logical: show repository read attempts (default: `!interactive()`)
#' * if `.tempdir==TRUE` then the downloaded zip file will be stored temporarily in [tempdir()]
#' * if `.tempdir==FALSE` then the downloaded zip file will be stored temporarily in [rappdirs::user_data_dir()]
#' * otherwise it is assumed that you give the name of an existing directory to store the downloaded zip file
#' @return invisibly the name of the current key
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
  if (length(args)==0) args <- list(mmstat$repo)
  stopifnot(length(args)==1)
  # determine key
  #browser()
  nargs <- names(args)
  if (is.null(nargs)) {
    key  <- normpathes(args[[1]])[[1]]
    file <- tail(key, 1)
    key  <- if (tools::file_ext(file) %in% c('zip')) tools::file_path_sans_ext(file) else file
  } else {
    key  <- nargs
  }
  # determine repos
  repos <- args[[1]]
  if (is.null(nargs)) { # key or file
    if (key %in% names(mmstat$repository)) {
      repos <- mmstat$repository[[key]]$url
    } else {
      repos <- c(repos, paste0(sprintf("https://github.com/%s/archive/refs/heads/", args[[1]]), c("main.zip", "master.zip")))
    }
  }
  # get download dir
  if (is.logical(.tempdir)) {
    ddir <- if (isFALSE(.tempdir)) user_data_dir('mmstat4') else tempdir()
  } else {
    ddir <- as.character(.tempdir)
  }
  #
  #  browser()
  repop  <- normpathes(repos)
  rfile  <- 0
  reposi <- NULL
  for (i in 1:length(repop)) {
    reposi <- repos[i]
#    if (!.quiet) cat (reposi, "\n")
    if (file.exists(reposi)) rfile <- i # local file exists
    try({ if (status_code(HEAD(reposi))==200) rfile <- -i }, silent = TRUE)
    if (rfile) {
      destfile <- paste0(gsub("[^[:alnum:]._]", "_", repop[[i]]), collapse="_")
      destfile <- paste(ddir, destfile, sep="/")
      if (.force || !file.exists(destfile)) {
        if (rfile>0) {
          file.copy(repos[i], destfile, overwrite=TRUE)
        }
        if (rfile<0) {
          res <- try(download.file(repos[i], destfile, quiet = TRUE), silent = TRUE)
          if (!inherits(res, 'try-error')) { fail <- FALSE; break }
        }
      }
      break
    }
  }
  if (rfile==0) {
    cat(paste0(repos, collapse="\n"))
    stop("None of the previously displayed possible ZIP files were found!")
  }
  if (!.quiet) {
    reposk <- mmstat$repository[[key]]$url
    if (is.null(reposk) || (reposk!=reposi)) {
      cat(paste0(repos, collapse="\n"), "\n")
      cat(sprintf('\nAdded: "%s"="%s"\n', key, reposi))
    }
  }
  # unzip, save key, repos, and build names
  mmstat$repository[[key]]$dir    <- ddir
  mmstat$repository[[key]]$url    <- reposi
  mmstat$repository[[key]]$files  <- normalizePath(unzip(destfile, exdir=ddir), winslash="/")
  mmstat$repository[[key]]$sfiles <- ghpath(ghdecompose(mmstat$repository[[key]]$files), "minpath")
  saveRDS(mmstat$repository, file=paste0(ddir, "/repositories.rds"), version=2)
  mmstat$repo <- key
  invisible(key)
}
