set.seed(1964)
n <- 100
x <- c(rnorm(n), 3+rnorm(n))

# ash.pdf
nbin <- 20
library("ash")
pdf("ash.pdf", bg="transparent", width=10, height=6)
par(mfrow=c(1,2))
diff=2
breaks=seq(-6, 6, diff)
hist(x, breaks, xlab='', freq=FALSE, col="darkgrey", ylim=c(0, 0.18), xlim=c(-7,7))
breaks=breaks+diff/4
hist(x, breaks, breaks[1], freq=FALSE, add=TRUE, border="red", col="coral")
breaks=breaks+diff/4
hist(x, breaks, breaks[1], freq=FALSE, add=TRUE, border="green", col="lightgreen")
breaks=breaks+diff/4
hist(x, breaks, breaks[1], freq=FALSE, add=TRUE, border="blue", col="lightblue")
#
b <- bin1(x, nbin=nbin)
#b0 <- b
#b0$nc <- rep(0, nbin)
#b0$nc[nbin/2] <-1
#f0 <- ash1(b0)
#plot(f0, type="s", main="Binned kernel (m=5)", xlab="", ylab="")
f <- ash1(b)
plot(f, type="s", main="Average shifted histogram", xlab="", ylab="", ylim=c(0, 0.18), xlim=c(-7,7))
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/ash.pdf"))