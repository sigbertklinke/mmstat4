addSurface <- function (plotobj, regobj, educ, exper) {
  for (i in 1:length(educ)) {
    newdata <- data.frame(educ=rep(educ[i], length(exper)), exper=exper)
    pred    <- predict(regobj, newdata=newdata)
    plotobj$points3d (newdata$educ, newdata$exper, pred, type="l", col="black")
  }
  for (i in 1:length(exper)) {
    newdata <- data.frame(educ=educ, exper=rep(exper[i], length(educ)))
    pred    <- predict(regobj, newdata=newdata)
    plotobj$points3d (newdata$educ, newdata$exper, pred, type="l", col="black")
  }
}

addContour <- function (regobj, educ, exper, ...) {
  nx <- length (educ)
  ny <- length (exper)
  z <- matrix(NA, ncol=nx, nrow=ny)
  for (i in 1:nx) {
    newdata <- data.frame(educ=rep(educ[i], length(exper)), exper=exper)
    z[i,]   <- predict(regobj, newdata=newdata)
  }
  contour(educ, exper, z, add=T, ...)
}

computeColors <- function (value, maxval, ncolor=50) {
  colpal <- colorRampPalette(c("blue", "white", "red"), space = "rgb")
  colors <- colpal(ncolor)
  colno  <- ceiling(ncolor*(maxval+value)/(2*maxval))
  colno[colno<1]      <- 1
  colno[colno>ncolor] <- ncolor
  return(colors[colno])
}

plotModel <- function (regobj, x, educ, exper, maxres, pp=list(cex=0.75), header=NULL, ...) {
  # two-dimensional grid 20x20
  educ  <- seq(min(x$educ), max(x$educ), length.out=20)
  exper <- seq(min(x$exper), max(x$exper), length.out=20)
  # plotting
  par(mfrow=c(1,2), oma=c(1,0,2,0))
  color <- computeColors(resid(regobj), maxres)
  n  <- length(x$educ)
  xr <- range(x$educ)
  yr <- range(x$exper)
  zr <- range(x$lwage)
  front <- c(T,F,T)
  resid <- resid(regobj)
  col   <- computeColors(resid, maxres)

  s3d<-scatterplot3d(xr, yr, zr, type="n", ...)
  s3d$points3d(x$educ, x$exper, rep(ifelse(front[3], zr[1], zr[2]), n), col="gray", cex=0.25)
  s3d$points3d(x$educ, rep(ifelse(front[2], yr[1], yr[2]), n), x$lwage, col="gray", cex=0.25)
  s3d$points3d(rep(ifelse(front[1], xr[1], xr[2]), n), x$exper, x$lwage, col="gray", cex=0.25)
  s3d$points3d(x$educ[resid<0], x$exper[resid<0], x$lwage[resid<0], col=color[resid<0], pch=19, cex=0.75)
  addSurface(s3d, regobj, educ, exper)
  s3d$points3d(x$educ[resid>=0], x$exper[resid>=0], x$lwage[resid>=0], col=color[resid>=0], pch=19, cex=0.75)

  plot(x$educ, x$exper, col=color, pch=19, xlab="educ", ylab="exper")
  addContour(regobj, educ, exper)
  if (!is.null(header)) {
    r2 <- 1-sum(resid(regobj)^2)/sum((x$lwage-mean(x$lwage))^2)
    mtext(sprintf("%s (R^2=%.3f)", header, r2), side=3, outer=T, cex=1.5)
  }
  if (!is.null(regobj$call)) { mtext(deparse(regobj$call), side=1, outer=T) }
}

dop <- T
library("scatterplot3d")
library("nnet")
library("rio")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]

size <- 10
file <- sprintf("mincer_nnet%.0f.pdf", size)
#for (i in seq(sizes)) {
  set.seed(0)
  nn <- nnet(lwage~educ+exper, data=x, maxit=minerrpos[i], size=size, linout=T)
  maxres <- max(abs(resid(nn)))
  pdf(file, width=10, height=6)
  plotModel (nn, x, educ, exper, maxres, header=sprintf("Neural network with %.0f hidden neurons", size))
  dev.off()
  if (interactive()) browseURL(paste0(getwd(), "/", file))
#}
