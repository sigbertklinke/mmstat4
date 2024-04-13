#' Creates a ZIP file or directory with files
#'
#' `ghzip` creates a ZIP file (if `dest` has an extension `zip`) or copies to the destination directory.
#' If `dest` is `NULL` then a temporary directory will be used.
#' Please note that neither the ZIP file is deleted nor the target directory is cleaned beforehand
#' if it already exists.
#'
#' @param files `ghdecompose` object or character: list of files to copy
#' @param dest character: ZIP file name of destination directory (default: `NULL`)
#'
#' @return the name of the destination directory or the ZIP file
#' @importFrom utils zip
#' @export
#'
#' @examples
#' if (interactive()) {
#'   zipfile <- tempfile(fileext='.zip')
#'   files   <- list.files(system.file(package="mmstat4"), recursive=TRUE)
#'   ghzip(files, zipfile)
#' }
ghzip <- function(files, dest=NULL) {
  is_absolute_path <- function(path) {
    is_windows_absolute <- grepl("^([A-Za-z]:|\\\\)", path)
    is_unix_absolute <- (substr(path, 1, 1) == "/")
    return(is_windows_absolute || is_unix_absolute)
  }
  #
  cp <- getwd()
  on.exit(setwd(cp))
  #
  if (!inherits(files, "ghdecompose")) files <- ghc(files)
  stopifnot(length(dest)<2)
  #
  tdir <- tempfile()
  if (length(dest)==1) {
    iszip <- (tolower(tools::file_ext(dest))=='zip')
    if (!iszip) tdir <- dest
  }
  tfiles <- paste(tdir, files$inpath, files$minpath, files$filename, sep='/')
  dirs   <- unique(dirname(tfiles))
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  file.copy(files$source, tfiles, copy.date=TRUE)
  if ((length(dest)==0) || !iszip) return(tdir)
  tfiles <- paste(files$inpath, files$minpath, files$filename, sep='/')
  tfiles <- gsub('//', '/', paste0(ifelse(substr(tfiles,1,1)=='/', '.' , './'), tfiles))
  if (!is_absolute_path(dest)) dest <- file.path(cp, dest)
  setwd(tdir)
  zip(dest, tfiles)
  return(dest)
}
