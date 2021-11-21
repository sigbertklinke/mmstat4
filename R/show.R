#' show
#'
#' Edits a program from the package \code{mmstat4} using \code{file.edit}.
#' Note that the file is copied to a temporary directory, such that you can not overwrite the original file!
#' If you do not save modified code at another location then the next \code{show} will overwrite your modified code.
#'
#' @param object character: name of example program
#'
#' @importFrom digest digest
#' @importFrom utils file.edit
#' @return nothing
#' @export
#'
#' @examples
#' if (interactive()) {
#'   show("stat/sum.R")
#' }
setMethod("show", "character",
          function(object) {
            object <- prg(object)
            if (length(object)>1) {
              warnmsg <- c("More than file found, taking first:\n",
                           paste0("  ", object, "\n"))
              warning(warnmsg)
            }
            file <- paste0(tempdir(), "/mmstat4/", digest(dirname(object), algo="crc32"), "_", basename(object))
            if (!dir.exists(dirname(file))) dir.create(dirname(file), recursive=TRUE)
            lines <- c(sprintf("# %s", file), readLines(object))
            writeLines(lines, file)
            file.edit(file)
          })
