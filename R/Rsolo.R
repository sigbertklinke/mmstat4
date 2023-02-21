#' Rsolo
#'
#' Checks whether all specified files are valid R files that can be executed independently of
#' each other. If an error occurs then:
#'
#' 1. If `open` is a function name or a function with a `file` parameter, then `Rsolo` will try to open the faulty R source file, otherwise not.
#' 2. The execution of `Rsolo` is stopped.
#'
#' If you do not want the faulty R-file to be opened immediately, use `open=0`
#'
#' @param files character: file name(s)
#' @param start integer: numeric index from which file to start (default: `1`)
#' @param path character: path to start from (default: `getwd()`)
#' @param open function: function or function name to call after an error occurs (default: `rstudioapi::navigateToFile`)
#' @param ... further parameters given to the function in `open`
#'
#' @return nothing
#' @importFrom rstudioapi navigateToFile
#' @export
#'
#' @examples
#' if (interactive()) {
#'   files <- list.files(pattern="*.R$", full.names=TRUE, recursive=TRUE)
#'   Rsolo(files)
#' }
Rsolo <- function(files, start=1, path=NULL, open=rstudioapi::navigateToFile, ...) {
  index <- 1:length(files)
  start <- as.integer(start)
  if (!all(start %in% index)) stop(sprintf("Invalid index in '%s'", paste0(start, collapse=",")))
  index <- if (length(start)==1) start:length(files) else start
  if (is.null(path)) path <- getwd()
  path  <- normalizePath(path, winslash="/")
  opath <- setwd(path)
  on.exit(setwd(opath))
  fun <- NULL
  if (is.function(open) || (is.character(open) && nchar(open))) {
    fun  <- try(match.fun(open), silent=TRUE)
    if (inherits(fun, "try-error")) {
      warning(sprintf("Function '%s' not found", as.character(open)))
      fun <- NULL
    }
  }
  for (i in index) {
    f <- files[i]
    print(sprintf("Rsolo %i: %s", i, f))
    setwd(dirname(f))
    res <- system(sprintf("Rscript %s", basename(f)), wait = TRUE)
    if (res!=0) {
      if (file.exists(f) && !is.null(fun)) {
        args      <- list(...)
        args$file <- f
        do.call(fun, args)
      }
      stop(sprintf("Rscript failed: %s", basename(f)))
    }
    setwd(path)
  }
}
