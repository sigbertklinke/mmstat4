set.seed(0)

n   <- 25
h   <- 0.2
x   <- rnorm(n)
hx  <- hist(x, plot=F)
pdf("kde.pdf")
rx  <- range(x)
xt  <- rx[1]+(-20:120)/100*diff(rx)
k   <- dnorm(outer(xt, x, "-")/h)
ke  <- apply(k, 1, sum)
jt  <- jitter(rep(-0.5, n), amount=0.5)
plot(xt, ke, type="l", col="red", main="Kernel density estimator", ylim=c(-1, max(ke)), lwd=3, ylab="Estimated density", xlab="x")
points(x, jt, pch=19)
for (i in 1:n) { lines(xt, k[,i], lwd=2) }
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/kde.pdf"))
