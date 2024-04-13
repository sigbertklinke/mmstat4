#' askUser
#'
#' `askUser` provides a way to ask the user a yes/no/cancel question (default). A `*` after a number indicates the default option.
#'
#' @param msg character: the prompt message for the user
#' @param choices	character: vector of choices (default: `c("Yes", "No", "Cancel")`)
#' @param default character/integer: default option if only `Enter` pressed (default: `1`)
#'
#' @return the integer number choosen by the user
#' @export
#'
#' @examples
#' if (interactive())
#'   askUser("Do you want to use askUser?")
askUser <- function(msg, choices=c("Yes", "No", "Cancel"), default=1) {
  def <- suppressWarnings(if (is.character(default)) which(choices==default) else as.integer(default))
  if (length(def)==0) def <- 0
  nchoices <- seq_along(choices)
  ask    <- sprintf("%s [%s]: ", msg, paste0(nchoices, ifelse(nchoices==def, "*=", '='), choices, collapse='/'))
  repeat {
    prompt <- readline(ask)
    if (nchar(prompt)==0) return(default)
    res <- suppressWarnings(as.integer(prompt))
    if (!is.na(res) && (res %in% nchoices)) break
    cat(sprintf("Please enter a number from 1, ..., %i or just <Enter>!\n", length(choices)))
  }
  return(res)
}
