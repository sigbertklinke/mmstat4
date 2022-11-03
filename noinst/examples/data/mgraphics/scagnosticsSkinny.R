rescale <- function (x) { 
  min <- apply(x,2,min)
  max <- apply(x,2,max)
  return(scale(x, center=min, scale=max-min))
}

library("scagnostics")
set.seed(0)
n  <- 100
n2 <- ceiling(0.2*n)
t  <- runif(n)
t2  <- runif(n-n2)
tn  <- (1:n)/n

x0  <- rescale(cbind(rnorm(n), rnorm(n)))
x1  <- rescale(cbind(runif(n), runif(n)))
x2  <- rescale(cbind(c(rnorm(n-n2), runif(n2)), c(rnorm(n-n2), runif(n2))))
x3  <- rescale(cbind(t, t+0.2*rnorm(n)))
x4  <- rescale(cbind(t, t+t/2*rnorm(n)))
x5  <- rescale(cbind(t, 0.1*rnorm(n)+ifelse(t<0.5, t/3, 2*(t-0.5))))
x6  <- rescale(cbind(t, sin(pi*t)+0.1*rnorm(n)))
x7  <- rescale(cbind(rnorm(n)/6+sin(2*pi/3*round(3*t)), rnorm(n)/6+cos(2*pi/3*round(3*t))))
x8  <- rescale(cbind(c(cos(2*pi*t2)+0.1*rnorm(n-n2), rnorm(n2)/5), c(sin(2*pi*t2)+0.1*rnorm(n-n2), rnorm(n2)/5)))
x9  <- rescale(cbind(tn, as.vector(arima.sim(list(order = c(1,1,0), ar = 0.7), n = n-1))+3*rnorm(n)))
x10 <- rescale(cbind(tn, sin(4*pi*tn)))
x11 <- rescale(cbind(round(5*runif(n))/5, runif(n)))

x    <- list(x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11)
scag <- rbind(scagnostics(x0), scagnostics(x1), scagnostics(x2), scagnostics(x3), scagnostics(x4), scagnostics(x5), scagnostics(x6), scagnostics(x7), scagnostics(x8), scagnostics(x9), scagnostics(x10), scagnostics(x11))

#for (i in 1:ncol(scag)) {
  i <- 7
  name <- sprintf("scagnostics%s.pdf", colnames(scag)[i])
  pdf(name, width=12, height=2)
  par(mfrow=c(1,12), mar=c(0,0,2,0), oma=c(0,0,2,0))
  scagi <- sort(scag[,i], index.return=T)
  for (j in 1:12) {
    plot(x[[scagi$ix[j]]], main=sprintf("%.4f", scagi$x[[j]]), axes=F)
    box()
  }
  mtext(colnames(scag)[i],  outer=T)
  dev.off()
#}
if (interactive()) browseURL(paste0(getwd(),"/", name))