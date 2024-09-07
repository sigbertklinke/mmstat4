#' @rdname pkglist
#' @aliases modlist
#' @aliases Rlibs
#' @title Extract `library` and `require` calls in R and `import` calls from Python
#' @description `pkglist` counts the number of `library`/`require`/`import` calls for
#' R and Python commands within the files.
#' It checks the availability of a package/module via [utils::available.packages()] (for R)
#' and via `PyPI` (for Python).
#' If `code=TRUE` is set, it returns R/Python code for installing packages/modules.
#' Otherwise, a table with the number of `library` or `import` calls is returned.
#'
#' @param files character: file name(s)
#' @param code logical: should names given back or code for init scrips? (default: `TRUE`)
#' @param repos character: the base URL(s) of the repositories to use (default: `getoption("repos")`)
#'
#' @return a table how frequently the packages are called or R Code to install them
#' @importFrom tools file_ext
#' @importFrom utils available.packages
#' @importFrom knitr purl
#' @export
#'
#' @examples
#' if (interactive()) {
#'   files <- list.files(pattern="*.(R|py)$", full.names=TRUE, recursive=TRUE)
#'   pkglist(files)
#' }
pkglist <- function(files, code=TRUE, repos=getOption("repos")) {
  Rlibs <- function(txt) {
    m <- unlist(regmatches(txt, gregexpr('(library|require)\\s*\\((.*?)\\)+', txt)))
    m <- gsub("(library|require)", "", m)
    m <- gsub("['\"()]", "", m)
#    browser(expr=('grid' %in% m))
    m
  }
  #
  pylibs <- function(txt) {
    lines <- trimws(unlist(strsplit(txt, "\\n")))
    lib <- NULL
    for (key in c("import", "from")) {
      ml  <- lines[startsWith(lines, paste(key, ""))]
      ml  <- strsplit(ml, ',', fixed=TRUE)
      lib <- c(lib, unlist(lapply(ml, key=key, function(e, key) {
        e <- strsplit(trimws(gsub(key, "", e)), "\\s+")
        sapply(e, '[', 1)
      })))
    }
    lib <- sapply(lib, strsplit, split='.', fixed=TRUE)
    lib <- unlist(sapply(lib, '[', 1))
    lib
  }
  #
  libs <- lapply(files, tmpfile = tempfile(), function(f, tmpfile) {
    ret <- list(R=NULL, py=NULL)
    if (file.exists(f)) {
      ext <- tolower(file_ext(f))
      txt <- paste0(readLines(f, warn=FALSE), collapse="\n")
      if (ext=='rmd') {
        purl(text=txt, output=tmpfile)
        txt <- paste0(readLines(tmpfile, warn=FALSE), collapse="\n")
      }
      if (ext %in% c('r', 'rmd')) ret$R <- c(ret$R, Rlibs(txt))
      if (ext %in% c('py', 'py3')) ret$py <- c(ret$py, pylibs(txt))
    }
    ret
  })
  #
  libs <- list(R=unlist(lapply(libs, function(x) x$R)), py=unlist(lapply(libs, function(x) x$py)))
  ret <- lapply(libs, table)
  if(!isTRUE(code)) return(ret)
  retcode <- NULL
  if (length(ret$R)) {
    if (length(repos)==0) repos <- "https://cloud.r-project.org"
    ap  <- NULL
    for (i in seq_along(repos)) ap <- rbind(ap, available.packages(repos=repos[i]))
    ap   <- ap[!duplicated(ap[,'Package']),]
    pos  <- match(names(ret$R), ap[,'Package'])
    retcode  <- c(retcode,
                  ifelse(is.na(ap[pos,'Repository']),
                         sprintf('# if(!require("%s")) install.packages("%s")\n', names(ret$R), names(ret$R)),
                         sprintf('if(!require("%s")) install.packages("%s", repos="%s")\n', names(ret$R), names(ret$R), ap[pos,'Repository'])
                  ))
  }
  if (length(ret$py)) {
    pkgavail <- urlExists(sprintf('https://pypi.org/pypi/%s/json', names(ret$py)))
    retcode <- c(retcode,
                 '# install.packages("reticulate")\n',
                 'library("reticulate")\n',
                 "venv <- py_env()\n",
                 sprintf('%sif(!py_module_available("%s")) py_install("%s", venv)\n', ifelse(pkgavail, "", "# "), names(ret$py), names(ret$py)))
  }
  names(retcode) <- NULL
  retcode
}

#' py_env
#'
#' Name of the currently used virtual emvironment.
#'
#' @return the name of the virtual Python environment currently used by `mmstat4`
#' @export
#'
#' @examples
#' py_env()
py_env <- function() { mmstat$repository[[mmstat$repo]]$venv }

#' @rdname pkglist
#' @export
Rlibs <- pkglist

#' @rdname pkglist
#' @export
modlist <- pkglist
