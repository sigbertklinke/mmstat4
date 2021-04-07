plotContour <- function (model, data, n=30) {
  mf <- model.frame(model$terms, data)
  mc <- lapply(as.list(mf[,-1]), pretty, n=n)
  zc <- predict(model, expand.grid(mc))
  dim(zc) <- sapply(mc, length)
  r2 <- 1-var(residuals(model))/var(mf[,1])
  contour(mc[[1]], mc[[2]], zc, xlab=names(mf)[2], ylab=names(mf)[3], 
          main=sprintf("R^2=%.3f", r2))
  cc <- gray(0.75-0.75*(mf[,1]-min(mf[,1]))/(max(mf[,1])-min(mf[,1])))
  points(mf[,2], mf[,3], pch=19, cex=0.5, col=cc)
}
#
data(Boston, package="MASS")
model <- lm(medv~lstat+rm, data=Boston)
par(mfrow=c(1,1))
plotContour(model, Boston)
par(mfrow=c(2,2))
plot(model)