#' @rdname packages
#' @aliases modules
#' @aliases Rlibs
#' @title Extract `library` and `require` calls in R and `import` calls from Python
#' @description `packages` counts the number of `library`/`require`/`import` calls for R and Python commands in the files.
#'
#' @param files character: file name(s)
#'
#' @return a table how frequently the packages are called
#' @importFrom tools file_ext
#' @export
#'
#' @examples
#' if (interactive()) {
#'   files <- list.files(pattern="*.(R|py)$", full.names=TRUE, recursive=TRUE)
#'   packages(files)
#' }
packages <- function(files) {
  Rlibs <- function(txt) {
    m <- unlist(regmatches(txt, gregexpr('(library|require)\\s*\\((.*?)\\)+', txt)))
    m <- gsub("(library|require)", "", m)
    m <- gsub("['\"()]", "", m)
    m
  }
  #
  pylibs <- function(txt) {
    lines <- trimws(unlist(strsplit(txt, "\\n")))
    lib <- NULL
    for (key in c("import", "from")) {
      ml  <- lines[startsWith(lines, paste(key, ""))]
      ml  <- strsplit(ml, ',', fixed=TRUE)
      lib <- c(lib, sapply(ml, key=key, function(e, key) {
        e <- strsplit(trimws(gsub(key, "", e)), "\\s+")
        sapply(e, '[', 1)
      }))
    }
    lib <- sapply(lib, strsplit, split='.', fixed=TRUE)
    unlist(sapply(lib, '[', 1))
  }
  #
  txts <- structure(lapply(files, function(f) { readChar(f, file.info(f)$size) }), names=files)
  libs <- list()
  for (f in names(txts)) {
    ext <- file_ext(f)
    libs[[ext]] <- c(libs[[ext]],
                     switch(ext,
                            R=Rlibs(txts[[f]]),
                            py=pylibs(txts[[f]]),
                            NULL))
  }
  lapply(libs, table)
}

#' @rdname packages
#' @export
Rlibs <- packages

#' @rdname packages
#' @export
modules <- packages
