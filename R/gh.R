#' @rdname gh
#' @title gh functions
#' @aliases ghopen ghload ghsource
#' @description
#' `gh` performs the operation described below on a file `x`.
#' A match for `x` is searched for the currently opened ZIP file.
#' If no unique match is found, then an error is thrown.
#' Otherwise, the following actions are performed:
#'
#' * `gh(x, 'open')` or `ghopen(x)`: Opens a file in the local browser if the file extension is `html` or `pdf`, otherwise in the RStudio editor.
#' * `gh(x, 'load')` or `ghload(x)`: Loads the contents of a file with `import`.
#' * `gh(x, 'source')` or `ghsource(x)`: Executes the contents of a file with `source`.
#'
#' @param x character(1): name of the file, app or data set
#' @param what character or function: a name of a predefined function or another function. The function must have a formal parameter `file`.
#' @param ... further parameters used in [utils::browseURL()], [rstudioapi::navigateToFile()], [rio::import()], or [base::source()].
#'
#' @return the result of [utils::browseURL], [rstudioapi::navigateToFile()], [rio::import()], or [base::source()].
#' @importFrom rstudioapi navigateToFile
#' @importFrom utils browseURL adist
#' @importFrom tools file_ext
#' @importFrom rio import
#' @export
#'
#' @examples
#' if (interactive()) {
#'   x <- ghopen("bank2.SAV")
#'   x <- ghload("bank2.SAV")
#'   str(x)
#'   x <- ghsource("univariate/example_ecdf.R")
#' }
gh <- function (x, what=c("open", "load", "source"), ...) {
  stopifnot(length(x)==1)
  file <- ghfile(x)
  ext  <- tolower(file_ext(file))
  if (is.character(what)) {
    fun <- switch(match.arg(what),
                  load=rio::import,
                  source=base::source,
                  if (ext %in% getOption("mmstat.ext.doc", c('html', 'pdf'))) utils::browseURL else rstudioapi::navigateToFile)
  }
  stopifnot(is.function(fun))
  ffun <- formals(fun)
  args <- list(...)
  if ('file' %in% names(ffun)) args$file <- file
  if ('url' %in% names(ffun)) args$url <- file
  do.call(fun, args)
}

#' @rdname gh
#' @export
ghopen <- function(x, ...) { gh(x, what='open', ...) }

#' @rdname gh
#' @export
ghload <- function(x, ...) { gh(x, what='load', ...) }

#' @rdname gh
#' @export
ghsource <- function(x, ...) { gh(x, what='source', ...) }
