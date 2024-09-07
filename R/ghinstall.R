#' ghinstall
#'
#' If the user agrees, it installs additional software necessary for running a script.
#' Currently, only `type=="py"` for Python scripts and `type=="R"`` for R scripts are supported.
#' When a repository is downloaded, `ghinstall` is called once. If the user calls `ghinstall` for an update,
#' the parameter `force=TRUE` must be set.
#'
#' @details
#' \describe{
#' \item{`R`}{`mmstat4` `init_R.R` is opened if present in the active repository.}
#' \item{`py`}{`mmstat4` internally utilizes a virtual environment named `mmstat4.xxxx`,
#' where xxxx`, varies depending on the repository.
#'  When `ghinstall` is invoked, it verifies the existence of the virtual environment
#'  `mmstat4.xxxx`. If it does not exist, the environment is created, and `init_py.R`
#'  is opened if present in the active repository.}
#  If the virtual environment `mmstat4` then exists, the file `py_install.py` is called
# (if it exists in the active repository. }
#' }
#'
#' @param type character: programm type (default: `py`)
#' @param force logical: should the installation really done (default: `NA)
#'
#' @return `NULL` if `type` is not found, otherwise `type`
#' @importFrom reticulate source_python virtualenv_exists virtualenv_remove virtualenv_create
#' @export
#'
#' @examples
#' # to delete the virtual environment use
#' # reticulate::virtualenv_remove('mmstat4')
#' if (interactive()) ghinstall()
ghinstall <- function(type=c("py", "R"), force=FALSE) {
  on.exit({
    display()
  })
  ret    <- NULL
  type   <- match.arg(type)
  doinst <- isTRUE(force) || isTRUE(mmstat$install[type])
  mmstat$install[type] <- FALSE
  if (doinst && (type=='R')) {
    instfile <- 'init_R.R'
    instr <- ghfile(instfile, silent=TRUE)
    if (length(instr)) {
      note("Note: The ZIP file creator included 'init_R.R', possibly requiring one-time execution for proper example functioning. It's now open in the editor.")
      openFile(instr[1])
    }
  }
  if (doinst && (type=='py')) {
    venv <- mmstat$repository[[mmstat$repo]]$venv
    # ask user
    msg    <- if (virtualenv_exists(venv)) sprintf("Recreate virtualenv '%s'", venv) else sprintf("Create virtualenv '%s'", venv)
    instm4 <- askUser(msg, default=3)
    #if (instm4==3) return(NULL)
    instfile <- 'init_py.R'
    instr <- ghfile(instfile, silent=TRUE)
    if (length(instr)) {
      note("Note: The ZIP file creator included 'init_py.R', possibly requiring one-time execution for proper example functioning. It's now open in the editor.")
      openFile(instr[1])
    }
#    dor <- 2
#    if (length(instr)) {
#      cat(inverse(instfile), "\n")
#      cat(green(paste(readLines(instr[1]), "\n")))
#      dor <- askUser(sprintf("Run '%s'", instfile))
#    }
#    if (dor==3) return(NULL)
#    instfile <- 'init_py.py'
#    instpy <- ghfile(instfile, silent=TRUE)
#    dopy <- 2
#    if (length(instpy)) {
#      cat(inverse(instfile), "\n")
#      cat(green(paste(readLines(instpy[1]), "\n")))
#      dopy <- askUser(sprintf("Run '%s'", instfile))
#    }
#    if (dopy==3) return(NULL)
#    #
    if (instm4==1) virtualenv_create(venv, packages=c("numpy", "scipy", "statsmodels", "pandas", "scikit-learn", "matplotlib", "seaborn"), force=TRUE)
    if (virtualenv_exists(venv)) use_virtualenv(venv) else warning(sprintf("If the virtualenv '%s' does not exist, Python scripts may not be executed correctly.", venv))
#    if (dor==1) source(instr[1], local = TRUE)
#    if (dopy==1) source_python(instpy[1], envir=NULL, convert=FALSE)
  }
}
