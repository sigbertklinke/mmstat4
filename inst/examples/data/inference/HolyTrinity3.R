# Unrestricted Likelihood L1
x    <- y <- (-20:20)/10
z    <- outer(dnorm(x), dnorm(y))
maxp <- (length(x)+1)/2 
# Restricted Likelihood hat{\theta}_x=x[k] L0
k  <- trunc(0.8*length(x))
xr <- rep(x[k],  length(y))
yr <- y
zr <- z[k,]
# Parameters: true, 0, 1
x0 <- x[k]; y0 <- y[maxp]; z0 <- z[k,maxp]
x1 <- x[maxp]; y1 <- y[maxp]; z1 <- z[maxp,maxp]
xt <- x[maxp+4]; yt <- y[maxp-3]; zt <- 0
#
library("plot.3d")
pdf("HolyTrinity3.pdf", width=12, height=6)
par(mfrow=c(1,2))
new3d("s", zlab="Likelihood") %>% 
  surface3d(x, y, z, xlab=expression(theta[x]), ylab=expression(theta[y]), col="grey") %>% 
  points3d(x1, y1, z1, pch=19) %>%
  text3d(x1, y1, z1, expression(hat(theta)[1]), pos=3) %>%
  lines3d(xr, yr, zr, col="blue") %>%
  points3d(x0, y0, z0, pch=19, col="blue") %>%
  text3d(x0, y0, z0, expression(hat(theta)[0]), pos=4, col="blue") %>%
  points3d(xt, yt, zt, pch=19, col="red") %>%
  text3d(xt, yt, zt, expression(theta[0]), pos=1, col="red") %>%  
  lines3d(c(x0,x1), c(y0,y1), c(z1,z1)) %>%
  lines3d(c(x0,x0), c(y0,y0), c(0,z0), col="coral") %>%
  arrows3d(x0,y0,0, xt,yt,0, col="red", length=0.1) %>%
  text3d((x0+xt)/2, (y0+yt)/2, 0, "Wald test", pos=1, col="red", cex=0.75) %>%  
  arrows3d(x0,y0,z0, x0,y0,z1, col="red", length=0.1) %>%
  text3d(x0, y0, (z0+z1)/2, "Likelihood ratio test", pos=4, col="red", cex=0.75) %>%  
  arrows3d(x0,y0,z0, x0+(x1-x0)/2, y0+(y1-y0)/2, z1, col="red", length=0.1) %>%
  text3d(x0+(x1-x0)/4, y0+(y1-y0)/4, (z0+z1)/2, "Score test", pos=2, col="red", cex=0.75) %>%  
  plot3d()
new3d("c", zlab="Likelihood") %>% 
  surface3d(x, y, z, xlab=expression(theta[x]), ylab=expression(theta[y])) %>% 
  points3d(x1, y1, z1, pch=19) %>%
  text3d(x1, y1, z1, expression(hat(theta)[1]), pos=3) %>%
  lines3d(xr, yr, zr, col="blue") %>%
  points3d(x0, y0, z0, pch=19, col="blue") %>%
  text3d(x0, y0, z0, expression(hat(theta)[0]), pos=4, col="blue") %>%
  points3d(xt, yt, zt, pch=19, col="red") %>%
  text3d(xt, yt, zt, expression(theta[0]), pos=1, col="red") %>%  
  arrows3d(x0,y0,0, xt,yt,0, col="red", length=0.1) %>%
  text3d((x0+xt)/2, (y0+yt)/2, 0, "Wald test", pos=1, col="red", cex=0.75) %>%  
  arrows3d(x0,y0,z0, x0+(x1-x0)/2,y0+(y1-y0)/2,z1, col="red", length=0.1) %>%
  text3d((x0+x1)/2, (y0+y1)/2, (z0+z1)/2, "Score test", pos=3, col="red", cex=0.75) %>%  
  plot3d()
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/HolyTrinity3.pdf"))
