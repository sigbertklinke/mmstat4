library("cluster")
diff <- c(1,2,3)
n    <- 100
col  <- c("blue", "red")
clu   <- 2:5 
pdf("Norm_diana.pdf", width=10, height=7, bg="transparent")
par(mfrow=c(length(diff), length(clu)), mar=c(0,0,0,0), mar=c(2,2,2,2), bg="transparent")
for (i in seq(diff)) {
  x1     <- matrix(rnorm(2*n), nr=n)
  x1[,1] <- x1[,1] + diff[i]
  x2     <- matrix(rnorm(2*n), nr=n)
  x2[,1] <- x2[,1] - diff[i]
  x      <- rbind(x1, x2)
  cl     <- diana(x)
  for (j in seq(clu)) {
    hcl <- cutree(as.hclust(cl), k = clu[j])
    plot(x, col=hcl, pch=19, axes=F, xlab="", ylab="",bg="transparent")
    box()
  }
}
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/Norm_diana.pdf"))