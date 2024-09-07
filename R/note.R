#' @rdname note
#' @aliases display
#' @title Create and display a note
#' @description `note` internally stores a colored message, while `display` utilizes [base::cat()] to present them
#' and reset the internal message stack.
#'
#' @param msg character: message
#' @param col function: a color function (default: `crayon::green`)
#'
#' @return `note` returns invisibly the number of notes
#' @export
#'
#' @examples
#' notetest <- function(msg) {
#'   on.exit({ display() })
#'   note(msg)
#'   # do some complex computation
#'   x <- 1+1
#' }
#' notetest("Hello world!")
note <- function(msg, col=crayon::green) {
  col <- match.fun(col)
  mmstat$notes <- c(mmstat$notes, col(msg))
  invisible(length(mmstat$notes))
}

#' @rdname note
#' @export
display <- function() {
  for (i in seq_along(mmstat$notes)) {
    eol <- endsWith(mmstat$notes[i], "\n")
    cat(paste0(mmstat$notes[i], ifelse(eol, '', "\n"), collapse=""))
  }
  mmstat$notes <- character()
}
