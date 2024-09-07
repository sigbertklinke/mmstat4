#' pkgMissing
#'
#' Checks if a `package` is available.
#'
#' @param package character: string naming the package/name space to load.
#'
#' @return a logical value
#' @export
#'
#' @examples
#' pkgMissing("tools")
#' pkgMissing("A3")
pkgMissing <- function(package) { !requireNamespace(package, quietly=TRUE) }
