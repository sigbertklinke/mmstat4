library("MASS")
set.seed(0)
n  <- 24
t  <- (1:n)/(n/2)
x <- list(cbind(t, t), cbind(t, -t),
          rbind(matrix(rnorm(n), nc=2), matrix(rnorm(n, mean=5), nc=2)),
          cbind(cos(pi*t), sin(pi*t)), rbind(matrix(rnorm(5*n), nc=2)))
lab <- list("Korrelation = +1", "Korrelation = -1", "Zwei Cluster", "Kreis", "Normalverteilung")

pdf("ParallelCoordinatePattern.pdf", width=10, height=4)
par(mfcol=c(2,length(x)), mar=c(0,0,2,0), pch=19)
for (i in seq(x)) {
  plot(x=x[[i]][,1], y=x[[i]][,2], axes=F)
  box()
  parcoord(x[[i]], main=lab[[i]])
  box()
}
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/ParallelCoordinatePattern.pdf"))
