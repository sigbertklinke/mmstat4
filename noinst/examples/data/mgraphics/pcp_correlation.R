library("MASS")
n <- 30
r <- c(-1, -0.8, 0, 0.8, 1)
pdf("pcp_correlation.pdf", width=10, height=6)
par(mfcol=c(2, length(r)))
for (ri in r) {
  repeat {
  	x <- mvrnorm(n=n, mu=c(0,0), Sigma=matrix(c(1,ri,ri,1), ncol=2))
  	if (abs(ri-cor(x)[1,2])<0.005) break
  }
  plot(0, 0, type="n", xlim=c(0,1), ylim=c(-3,3), axes=F, main="Pcp", 
  		 xlab="", ylab="")
  abline(v=c(0,1))
  for (i in 1:n) lines(c(0,1), x[i,])
  axis(1, at=c(0,1), labels=c("x1", "x2"))
  axis(2)
  box()
	plot(x, pch=19, xlim=c(-3,3), ylim=c(-3,3), asp=T, xlab="x1", ylab="x2", 
			 main="2D", sub=sprintf("r = %.2f", cor(x)[1,2]))
}
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/pcp_correlation.pdf"))