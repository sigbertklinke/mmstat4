fulldata <- function (x) {
  xn <- names(x)
  y  <- vector("list", length(xn))
  names(y) <- xn
  for (i in 1:nrow(xw)) {
    if (x$Freq[i]>0) {
      for (name in xn) y[[name]] <- c(y[[name]], rep(x[[name]][i], x$Freq[i]))
    }
  }

  for (name in xn) {
    if (class(x[[name]])=="factor") {
      y[[name]] <- factor(y[[name]], labels=levels(x[[name]]))
    }
  }
  as.data.frame(y)
}

xw <- as.data.frame(HairEyeColor)
xf <- fulldata(xw)

