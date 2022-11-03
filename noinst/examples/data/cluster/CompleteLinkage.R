baseplot <- function (x1, x2, x3) {
  ellipse <- function (x0, y0, a, b) {
    theta <- seq(0, 2 * pi, length=100)
    x <- x0 + a * cos(theta)
    y <- y0 + b * sin(theta)
    polygon(x, y, col="grey95", border=NA)  
  }

  par(mar=c(0,0,0,0))
  xlim <- range(c(x1[,1], x2[,1], x3[,1]))*1.2
  ylim <- range(c(x1[,2], x2[,2], x3[,2]))*1.2
  plot(0,0 , xlim=xlim, ylim=ylim, type="n", asp=T, xlab="", ylab="", axes=F)
  ellipse(1, 0, 0.5, 0.6)
  text(1, 0, "B")
  text(0, 0.2, "A")
}

set.seed(3)
n1   <- 3
x1   <- matrix(rnorm(2*n1)/7, ncol=2)
n2   <- 6
x2   <- matrix(rnorm(2*n2)/7, ncol=2)+c(rep(0.9,n2), rep(+0.25, n2))
n3   <- 3
x3   <- matrix(rnorm(2*n3)/7, ncol=2)+c(rep(1.1,n3), rep(-0.25, n3))
x    <- rbind(x1, x2, x3)
cx   <- c(rep("red", n1), rep("blue", n2), rep("green", n3))
d    <- as.matrix(dist(x))

# Complete linkage
pdf("CompleteLinkage.pdf", width=3, height=3)
pos  <- which(d[1:n1, -(1:n1)] == max(d[1:n1, -(1:n1)]), arr.ind = TRUE)
baseplot(x1, x2, x3)
lines(c(x[pos[1],1], x[pos[2]+n1,1]), c(x[pos[1],2], x[pos[2]+n1,2]))
points(x, pch=19, col=cx)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/CompleteLinkage.pdf"))