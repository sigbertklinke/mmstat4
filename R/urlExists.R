#' urlExists
#'
#' Verifies whether a provided `url` is downloadable, without detecting redirections in the URL.
#'
#' @param url a vector of text URLs
#'
#' @return `TRUE` if URL exists otherwise `FALSE`
#' @importFrom httr HEAD status_code
#' @export
#'
#' @examples
#' if (interactive()) {
#'   urlExists("https://hu-berlin.de/sk")
#'   urlExists("https://huglawurza.de")
#' }
urlExists <- function(url) {
  ret <- rep(FALSE, length(url))
  url <- as.character(url)
  for (i in seq_along(url)) {
    suppressWarnings(response <- try(HEAD(url[i]), silent=TRUE))
    if (!inherits(response, 'try-error')) ret[i] = (status_code(response) == 200)
  }
  ret
}
