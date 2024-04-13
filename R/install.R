#' install
#'
#' If a R package or Python module is not already installed, it will be installed with the user's consent.
#'
#' @param rlibs character: names of R packages
#' @param pymods character: names of Python modules
#'
#' @return invisibly a list of the R packages and Python modules that were attempted to be installed
#' @importFrom reticulate py_install py_module_available use_virtualenv
#' @importFrom utils install.packages
#' @export
#'
#' @examples
#' if (interactive()) {
#'   install(rlibs="A3", pymods="numpy")
#' }
install <- function(rlibs=NULL, pymods=NULL) {
  #browser()
  if (length(rlibs)) {
    for (lib in rlibs) {
      if (requireNamespace(lib, quietly=TRUE)) rlibs <- setdiff(rlibs, lib)
    }
  }
  if (length(pymods)) {
    for (mod in pymods) {
      if (suppressMessages(py_module_available(mod))) pymods <- setdiff(pymods, mod)
    }
  }
  dorlibs <- dopymods <- 2 # 2=No
  if (length(rlibs))  dorlibs <- askUser("R package(s) missing, install")
  if (length(pymods)) dopymods <- askUser("Python module(s) missing, install")
  if (dorlibs==1) {
    for (lib in rlibs) {
      try ({
        install.packages(lib, character.only=TRUE)
      })
    }
  }
  if (dopymods==1) {
    use_virtualenv(mmstat$repository[[mmstat$repo]]$venv)
    py_install(pymods, mmstat$repository[[mmstat$repo]]$venv)
  }
  invisible(list(R=rlibs, py=pymods))
}

#ret    <- NULL
#check  <- c("R"="require", "py"="py_module_available")
#inst   <- c("R"="install.packages", "py"="py_install")
#title  <- c("R"="R package(s) missing, install", "py"="Python module(s) missing, install")
#nolibs <- NULL
#for (lib in libs) {
#  ok <- suppressMessages(do.call(check[type], list(lib)))
#  if(ok) nolibs <- c(nolibs, lib)
#}
#libs   <- setdiff(libs, nolibs)
#doinst <- NA
#if (length(libs)) {
#  doinst <- askUser(title[type])
#}
#
#if (is.na(doinst)) {
#
#  if (doinst==3) stop("'install' cancelled")
#}
#if (doinst==1) {
#  do.call(inst[type], list(lib))
#  ret <- c(ret, lib)
#} #jans comment: mmxssdnnnnnnnnnnnnnnhrrrrrrrrrrrrrrrrreeeeeeeh4ehhhhhhhhe3                   vcba vbbbbb
#}
#}
#  invisible(ret)
#}
