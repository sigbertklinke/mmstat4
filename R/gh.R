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
#' * `gh(x, 'app')` or `ghapp(x)`: Tries to open the file with the default application of the OS, see [defaultApp()].
##'
#' @param x character(1): name of the file, app or data set
#' @param what character or function: a name of a predefined function or another function. The function must have a formal parameter `file`.
#' @param ... further parameters used in [utils::browseURL()], [rstudioapi::navigateToFile()], [rio::import()], or [base::source()].
#'
#' @return invisibly the result of [utils::browseURL], [rstudioapi::navigateToFile()], [rio::import()], or [base::source()].
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
gh <- function (x, what=c("open", "load", "source", "app"), ...) {
  stopifnot(length(x)==1)
  file <- ghfile(x)
  ext  <- tolower(file_ext(file))
  if (is.character(what)) {
    fun <- switch(match.arg(what),
                  load=rio::import,
                  source=base::source,
                  app=mmstat4::defaultApp,
                  if (ext %in% getOption("mmstat.ext.doc", c('html', 'pdf'))) utils::browseURL else rstudioapi::navigateToFile)
  }
  stopifnot(is.function(fun))
  ffun <- formals(fun)
  args <- list(...)
  if ('file' %in% names(ffun)) args$file <- file
  if ('url' %in% names(ffun)) args$url <- file
  invisible(do.call(fun, args))
}

#' defaultApp
#'
#' Tries to open the given `file` with the default application of the operating system using [base::system2()].
#' Only Windows (`windows`), macOS (`darwin`), Linux (`linux`) and FreeBSD (`freebsd`) is supported.
#'
#' @param file character: file name
#' @param wait logical: indicates whether the R interpreter should wait for the command to finish, or run it asynchronously (default: `FALSE`)
#' @param ... further arguments passed to `system2`
#'
#' @seealso \href{https://CRAN.R-project.org/package=berryFunctions}{`berryFunctions::openFile()`}
#'
#' @return Result of `try(system2, ...)`, invisibly
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ghget()
#'   defaultApp(ghlist("dataanalysis.pdf", full.names = TRUE))
#' }
defaultApp <- function(file, wait=FALSE, ...) {
  file <- normalizePath(file, winslash="/", mustWork=FALSE)
  if (!file.exists(file)) stop(sprintf("File not found: %2", file))
  file <- shQuote(file) # to handle space in "C:/Program Files/R/..."
  sys  <- tolower(Sys.info()["sysname"])
  if (sys=="linux") {
    linux <- Sys.which(c("xdg-open", "gnome-open"))
    linux <- linux[which.min(nchar(linux)==0)]
  }
  out <- try(switch(sys,
                    "linux"   = system2(linux, file, wait=wait, ... ),
                    "freebsd" = system2("handlr", paste("open", file),  wait=wait, ...),
                    system2("open", file,  wait=wait, ...)  # Windows or Mac
  ), silent=TRUE)
  # out: 127 if failed, 124 for timeout, 0 for success
  return(invisible(out))
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

#' @rdname gh
#' @export
ghapp <- function(x, ...) { gh(x, what='app', ...) }
