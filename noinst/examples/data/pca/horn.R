library("rio")
library("boot")
data("bank2", package="mmstat4")
bank2    <- scale(bank2)
evx  <- prcomp(bank2)$sdev^2
xdim <- dim(bank2)

eigenvalue <- function(x, ind) { # parameter is required by boot(!)
  xb <- matrix(rnorm(prod(dim((x)))), nrow=nrow(x), ncol=ncol(x))
  prcomp(xb)$sdev^2
}

set.seed(0)
B  <- 5000
ev <- boot(bank2, eigenvalue, R=B)
d <- vector("list", ncol(bank2))
xlim <- range(evx)
ylim <- c(0,0)
for (i in 1:ncol(bank2)) {
  d[[i]] <- density(ev$t[,i])
  xlim   <- range(c(xlim, d[[i]]$x))
  ylim   <- range(c(ylim, d[[i]]$y))
}

pdf("horn.pdf", width=10, height=7)
plot(mean(xlim), mean(ylim), xlim=xlim, ylim=ylim, type="n", ylab="",
     xlab="observed (dashed), bootstrapped from N(0;I) (solid)",
     main="Parallel analysis by Horn")
col <- rainbow(ncol(bank2))
for (i in 1:ncol(bank2)) {
  lines(d[[i]], col=col[i])
  abline(v=evx[i], col=col[i], lty="dashed")
#  abline(v=mean(ev$t[,i]), col=col[i], lty="dashed")
#  abline(v=quantile(ev$t[,i], 0.95), col=col[i], lty="dotted")
}
legend(x=1.8, y=8, sprintf("%.0f. eigenvalue", 1:ncol(bank2)), col=col, lwd=2)
dev.off()

library("xtable")
tab <- xtable(data.frame(ev.data=evx, ev.horn.mean=colMeans(ev$t), ev.horn.q95=apply(ev$t, 2, quantile, probs=0.95)))
digits(tab) <- 3
caption(tab) <- sprintf("n=%i, p=%i, B=%i", xdim[1], xdim[2], B)
print(tab, file="horn.tex")
if (interactive()) browseURL(paste0(getwd(),"/horn.pdf"))

