#' show
#'
#' Edits a program from the package \code{mmstat} using \code{file.edit}.
#'
#' @param object character: name of example program
#'
#' @return nothing
#' @importFrom utils file.edit
#' @export
#'
#' @examples
#' if (interactive()) {
#'   show("stat/sum.R")
#' }
setMethod("show", "character",
          function(object) {
            fed <- try(match.fun("file.edit"), silent = TRUE)
            if ("try-error" %in% class(fed)) fed <- file.edit
            file <- paste0(tempdir(), "/", object)
            if (!dir.exists(dirname(file))) dir.create(dirname(file), recursive=TRUE)
            file.copy(prg(object), file, overwrite=TRUE)
            fed(file)
          })
