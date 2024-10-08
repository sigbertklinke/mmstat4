#' @rdname checkFiles
#' @aliases Rsolo
#' @title Checks whether all specified files are valid R or Python files
#' @description `checkFiles` verifies whether all specified files are valid source files
#' that can be executed independently of each other. If an error occurs, the following actions are taken:
#'
#' 1. If `open` is either a function name or a function with a `file` parameter, then `checkFiles`
#'    will attempt to open the faulty source file; otherwise, it will not.
#' 2. The execution of `checkFiles` is stopped.
#'
#' If you do not want the faulty source file to be opened immediately, use `open=0`.
#'
#' Three modes are available for checking a `file`:
#'
#' 1. `exist`: Does the source file exist?
#' 2. `parse`: (default) Is `parse(file)` (in R) or `python -m py_compile "file"` (in Python) successful?
#' 3. `run`: Is `Rscript "file"` (in R) or `reticulate::py_run_file(file)` (in Python) successful?
#'
#' If source files have side effects, e.g., generating an image or producing other outputs,
#' and `mode == "parse"`, these side effects will occur during the check.
#' To prevent a script from being executed during the check, add a `## Not check:` comment at the top of the script.
#'
#' @param files character: file name(s)
#' @param index integer(s):  if `length(index)==1` the files from `index` to `length(files)` are checked (default: `seq_along(files)`) otherwise the files with values in `index` are checked.
#' @param path character: path to start from (default: `getwd()`)
#' @param open function: function or function name to call after an error occurs (default: `openFile`)
#' @param mode character which check to do
#' @param ... further parameters given to the function in `open`
#'
#' @return nothing
#' @importFrom reticulate py_discover_config py_run_file
#' @importFrom tools file_ext
#' @export
#'
#' @examples
#' if (interactive()) {
#'   files <- list.files(pattern="*.(R|py)$", full.names=TRUE, recursive=TRUE)
#'   checkFiles(files)
#' }
checkFiles <- function(files, index=seq_along(files), path=NULL, open=openFile, mode=c('parse', 'run', 'exist'), ...) {
  # which files to test
  index <- as.integer(index)
  if (length(index)==1) index <- index[1]:length(files)
  index <- intersect(index, seq_along(files))
#  ind   <- setdiff(index, seq_along(index))
#  if (length(ind)) stop(sprintf("Invalid indices: '%s'", paste0(ind, collapse=", ")))
  # set path
  if (is.null(path)) path <- getwd()
  path  <- normalizePath(path, winslash="/")
  opath <- setwd(path)
  on.exit(setwd(opath))
  # set open function
  fun <- NULL
  if (is.function(open) || (is.character(open) && nchar(open))) {
    fun  <- try(match.fun(open), silent=TRUE)
    if (inherits(fun, "try-error")) {
      warning(sprintf("Function '%s' not found", as.character(open)))
      fun <- NULL
    }
  }
  # check each file
  mode <- match.arg(mode)
  fext <- file_ext(files)
  if (any('py' %in% fext)) pyc <- py_discover_config(use_environment="mmstat4")
  for (i in index) {
    stopi <- FALSE
    f     <- files[i]
    cat(sprintf("file %i: %s\n", i, f))
    if (!file.exists(f)) stop("file does not exist")
    setwd(dirname(f))
    if (mode=='parse') {
      if (fext[i]=="R")  stopi <- inherits(try(parse(basename(f))), "try-error")
      if (fext[i]=="py") {
        cmd   <- sprintf('%s -m py_compile "%s"', pyc$executable, f)
        stopi <- system(cmd, wait = TRUE)
      }
    }
    if (mode=='run') {
      donotrun <- any(grepl('## Not check:', suppressWarnings(readLines(f))))
      if ((fext[i]=="R") && !donotrun)  stopi <- system(sprintf('Rscript "%s"', basename(f)), wait = TRUE)
      if ((fext[i]=="py") && !donotrun) {
        res   <- try(reticulate::py_run_file(f))
        stopi <- inherits(res, "try-error")
      }
    }
    setwd(path)
    if (stopi) {
      args      <- list(...)
      args$file <- f
      do.call(fun, args)
      stop(sprintf("checkFile failed: %s", basename(f)))
    }
  }
}

#' @rdname checkFiles
#' @export
Rsolo <- checkFiles

#Rsolo <- function(files, start=1, path=NULL, open=rstudioapi::navigateToFile, ...) {
#  index <- 1:length(files)
#  start <- as.integer(start)
#  if (!all(start %in% index)) stop(sprintf("Invalid index in '%s'", paste0(start, collapse=",")))
#  index <- if (length(start)==1) start:length(files) else start
#  if (is.null(path)) path <- getwd()
#  path  <- normalizePath(path, winslash="/")
#  opath <- setwd(path)
#  on.exit(setwd(opath))
#  fun <- NULL
#  if (is.function(open) || (is.character(open) && nchar(open))) {
#    fun  <- try(match.fun(open), silent=TRUE)
#    if (inherits(fun, "try-error")) {
#      warning(sprintf("Function '%s' not found", as.character(open)))
#      fun <- NULL
#    }
#  }
#  for (i in index) {
#    f <- files[i]
#    print(sprintf("Rsolo %i: %s", i, f))
#    setwd(dirname(f))
#    res <- system(sprintf("Rscript %s", basename(f)), wait = TRUE)
#    if (res!=0) {
#      if (file.exists(f) && !is.null(fun)) {
#        args      <- list(...)
#        args$file <- f
#        do.call(fun, args)
#      }
#      stop(sprintf("parseFile failed: %s", basename(f)))
#    }
#    setwd(path)
#  }
#}
