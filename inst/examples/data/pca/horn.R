library("foreign")
library("boot")
x <- import("https://shinyapps.wiwi.hu-berlin.de/d/BANK2.sav")
xdim <- dim(x)
evx  <- prcomp(x, center=T, scale=T)$sdev^2

eigenvalue <- function(x, ind) {
  xb <- matrix(rnorm(prod(dim(x))), nrow=nrow(x), ncol=nrow(x))
  prcomp(xb)$sdev^2 
}

set.seed(0)
ev<-boot(x, eigenvalue, R=5000)

pdf("horn.pdf", width=10, height=7)
par(mfrow=c(2,3), mar=c(4,0,3,1))
evr <- range(rbind(ev))+0.1*c(-1,1)
for (i in 1:6) {
  hist(ev[,i], xlim=evr, main=sprintf("%ith eigenvalue", i), axes=F, xlab=sprintf("From bank note data: %.2f", evx[i]), col="gray")
  mev <- mean(ev[,i])
  q95 <- quantile(ev[,i], 0.95)
  abline(v=mev, col="blue", lwd=2)
  abline(v=q95, col="blue", lwd=2)
  abline(v=evx[i], col="red", lwd=2)
  axis(1)
}
dev.off()

library("xtable")
tab <- xtable(data.frame(ev.data=evx, ev.horn.mean=colMeans(ev), ev.horn.q95=apply(ev, 2, quantile, probs=0.95)))
digits(tab) <- 3
caption(tab) <- sprintf("n=%i, p=%i, B=%i", xdim[1], xdim[2], B)
print(tab, file="horn.tex")
