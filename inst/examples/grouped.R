grouped <- function(obj, ...) UseMethod("grouped")
linfun  <- function(obj, ...) UseMethod("linfun")

grouped.default <- function (obj, ...) {
  args <- list(...)
  if (is.null(args$plot)) args$plot <- FALSE
  if (is.null(args$x)) args$x <- obj
  ret <- do.call("hist", args)
  ret$xname <- paste(deparse(substitute(obj), 500), collapse = "\n")
  class(ret) <- c("grouped")
  return(ret)
}

is.grouped <- function(obj) { "grouped" %in% class(obj) }

plot.grouped <- function(obj, ...) {
  pobj <- obj
  class(pobj) <- "histogram"
  plot(pobj, ...)
}

print.grouped <- function(obj, ...) {
  pobj <- obj
  class(pobj) <- "histogram"
  print(pobj, ...)
}

prettyNumbers <- function(x) {
  ret <- rep("", length(x))
  if (sum(x<0)) ret <- paste0(ret, ifelse(x<0, "-", "+"))
  tx  <- trunc(abs(x))
  ret <- paste0(ret, sprintf("%*i", max(nchar(tx)), tx))
  fx  <- abs(x)-tx
  if(!isTRUE(all.equal(fx, rep(0, length(fx))))) {
    dec <- sprintf("%.*f", max(nchar(fx)-2), fx)
    ret <- paste0(ret, substring(dec, 2))
  }
  attr(ret, "nchar") <- max(nchar(ret))
  ret
}

prettyHeader <- function(fmt, x, n) {
  args <- vector("list", length(x))
  nr   <- rep(n, length.out=length(x))
  for (i in seq(x)) {
    args[[i]] <- sprintf("%*s", nr[i], abbreviate(x[i], minlength=nr[i]))
  }
  args$fmt <- fmt
  do.call("sprintf", args)
}

abs.freq <- function (obj) {
  if (!is.grouped(obj)) stop("")
  obj$counts
}

rel.freq <- function (obj) {
	if (!is.grouped(obj)) stop("")
	obj$counts/sum(obj$counts)
}

density <- function (obj) {
	if (!is.grouped(obj)) stop("")
	obj$density
}

mids <- function (obj) {
  if (!is.grouped(obj)) stop("")
  obj$mids
}

lower <- function (obj, x=NULL) {
  if (!is.grouped(obj)) stop("")
  if (is.null(x)) return(obj$breaks[-length(obj$breaks)])
  ret  <- rep(-Inf, length(x))
  i    <- apply(outer(obj$breaks, x, "<="), 2, sum)
  cond <- (i>0) 
  ret[cond] <- obj$breaks[i[cond]]
  return(ret)
}

upper <- function (obj, x=NULL) {
  if (!is.grouped(obj)) stop("")
  if (is.null(x)) return(obj$breaks[-1])
  ret <- rep(+Inf, length(x))
  i   <- 1+apply(outer(obj$breaks, x, "<="), 2, sum)
  cond <- (i<=length(obj$breaks)) 
  ret[cond] <- obj$breaks[i[cond]]
  return(ret) 
}

width <- function (obj) {
	if (!is.grouped(obj)) stop("")
	diff(obj$breaks)
}

summary.grouped <- function(obj, ...) {
  breaks <- prettyNumbers(obj$breaks)
  f      <- obj$counts/sum(obj$counts)
  data.frame(lower = breaks[-length(breaks)],
             upper = breaks[-1],
             hi    = prettyNumbers(obj$counts),
             Hi    = prettyNumbers(cumsum(obj$counts)),
             fi    = prettyNumbers(f),
             Fi    = prettyNumbers(cumsum(f)),
             fhati = prettyNumbers(obj$density)
            )
}

mean <- function(obj) {
	sum(obj$mids*obj$counts)/sum(obj$counts)
}

dgrouped <- function(x, obj) {
 sf <- stepfun(obj$breaks, c(0, obj$density, 0))
 sf(x)
}

pgrouped <- function(x, obj) {
  cdf <- c(0,cumsum(obj$density*(upper(obj)-lower(obj))))
  str(cdf)
  sf  <- approxfun(obj$breaks, cdf, yleft=0, yright=1)
  sf(x)
}

ecdf.grouped <- function (obj) {
  rval <- approxfun(obj$breaks, c(0, cumsum(obj$counts/sum(obj$counts))), yleft = 0, yright = 1)
  class(rval) <- c("linfun", class(rval))
  attr(rval, "breaks") <- obj$breaks
  attr(rval, "call")   <- sys.call()
  rval
}

plot.linfun <- function(objf, type="l", pch = par("pch"), lty = par("lty"), col = par("col"), 
                        bg = NA, cex = 1, lwd = par("lwd"), xlab="x", ylab="F(x)", 
                        main=attr(objf, "call"),
                        ...) {
  plot(attr(objf, "breaks"), objf(attr(objf, "breaks")),
       type=type, pch=pch, lty=lty, col=col, bg=bg, cex=cex, lwd=lwd, xlab=xlab, ylab=ylab, main=main,
       ...)
  abline(h=c(0,1), lty="dotted")
}

quantile.grouped <- function(obj, probs = seq(0, 1, 0.25)) {
  fx  <- rel.freq(obj)
	Fx  <- cumsum(fx)
	j   <- 1+apply(outer(Fx, probs, "<="), 2, sum)
  cdf <- ecdf.grouped(obj)
  xu  <- lower(obj, obj$breaks[j])
  q   <- xu+(probs-cdf(xu))/fx[j]*width(obj)[j]
  q   <- ifelse(is.na(q), 1, q)
  ifelse((probs<0)|(probs>1), NA, q)
}

median.grouped <- function(obj) { quantile(obj, 0.5) }
	


# Test
x <- grouped(rnorm(1000))
plot(x)