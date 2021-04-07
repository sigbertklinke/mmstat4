#' @rdname cdf
#' @title Generates and plots a cumulative distribution function.
#'
#' @param x numeric: x-values
#' @param y numeric: y-values
#' @param discrete logical: if distribution is discrete
#' @param col.01line color: color of horizontal lines at 0 and 1 (default: \code{black})
#' @param pch point type: See [graphics::points()] for possible values and their interpretation (default: \code{19})
#' @param ... further parameters given to [graphics::plot()]
#'
#' @return returns a `cdf` object
#' @export
#'
#' @examples
#' # Binomial distribution
#' x <- cdf(0:10, pbinom(0:10, 10, 0.5))
#' plot(x)
#' # Exponential distribution
#' x <- seq(0, 5, by=0.01)
#' x <- cdf(x, pexp(x), discrete=FALSE)
#' plot(x)
cdf <- function(x, ...) { UseMethod("cdf") }

#' @rdname cdf
#' @method cdf default
#' @export
cdf.default <- function(x, y, discrete=TRUE, ...) {
  o <- order(x)
  structure(list(x=x, y=y, discrete=discrete), class="cdf")
}

#' @rdname cdf
#' @importFrom graphics points lines abline
#' @export
plot.cdf <- function(x, y, ..., col.01line = "black", pch=19) {
  args <- list(...)
  if (is.null(args$xlim)) args$xlim <- range(x$x)
  if (is.null(args$ylim)) args$ylim <- c(0,1)
  if (is.null(args$xlab)) args$xlab <- deparse(substitute(x))
  if (is.null(args$ylab)) args$ylab <- sprintf("F(%s)", args$xlab)
  args$type <- "n"
  args$x    <- mean(x$x)
  args$y    <- mean(x$y)
  do.call("plot", args)
  xl <- c(min(x$x)-diff(args$xlim), x$x)
  xr <- c(x$x, max(x$x)+diff(args$xlim))
  y  <- c(0, x$y)
  if(x$discrete) {
    for (i in seq(xl)) lines(c(xl[i], xr[i]), c(y[i], y[i]))
    points(x$x, x$y, pch=pch)
  } else {
    xl <- c(xl, max(x$x)+diff(args$xlim))
    y  <- c(y, 1)
    lines(xl, y)
  }
  abline(h=c(0,1), lty=2, col=col.01line)
}
