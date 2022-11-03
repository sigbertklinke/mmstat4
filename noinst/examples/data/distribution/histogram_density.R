pdf("histogram_density.pdf", width=10)
par(mfrow=c(1,2))
xlim <- c(-10,15)
ylim <- c(0, 0.17)
x <- c(-2.5, -1.5, -0.5, 1.5, 5.2, 6.2)
hist(x, xlim=xlim, ylim=ylim, freq = FALSE)
rug(x, lwd=3)
d<-density(x, bw=1.5)
plot(d, xlim=xlim, ylim=ylim)
xi <- min(d$x)+diff(range(d$x))*(0:200)/200
for (i in seq(x)) {
  lines(xi, 1/5*dnorm(xi, mean=x[i], sd=d$bw), col="blue", lty="dashed")
}
rug(x, lwd=3)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/histogram_density.pdf"))
