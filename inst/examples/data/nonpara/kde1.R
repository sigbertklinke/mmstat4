set.seed(0)
n   <- 25
x   <- rnorm(n)
h   <- 0.1
rx  <- range(x)
xt  <- rx[1]+(-20:120)/100*diff(rx)
#
pdf("kde1.pdf")
k   <- dnorm(outer(xt, x, "-")/h)
ke  <- apply(k, 1, sum)
jt  <- jitter(rep(-0.5, n), amount=0.5)
ylim <- c(-1, max(ke))
plot(xt, ke, type="l", col="red", main=sprintf("Kernel density estimator (h=%.1f)", h), ylim=ylim, lwd=3, ylab="Estimated density", xlab="x")
points(x, jt, pch=19)
for (i in 1:n) { lines(xt, k[,i], lwd=2) }
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/kde1.pdf"))
