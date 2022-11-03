library("mlbench")
library("alphahull")
library("igraph")

data(BostonHousing)

x <- cbind(BostonHousing$lstat, BostonHousing$medv)
x <- unique(x)

pdf("scagnosticsBase.pdf", width=10, height=3)
par(mfrow=c(1,3), mar=c(0,0,2,0))

hull  <- chull(x)
shape <- ashape(x, alpha=5)

hull <- c(hull, hull[1])

plot(x, main="Convex hull", axes=F, pch=19, cex=0.5)
lines(x[hull, ])
box()

plot(x, main="Alpha shape", axes=F, pch=19, cex=0.5)
plot(shape, add=T, wpoints=F)
box()

graph <- graph.adjacency(as.matrix(dist(x)), weighted=TRUE)
mst   <- as.undirected(minimum.spanning.tree(graph))
idx   <- get.edges(mst, E(mst))
plot(x, main="Minimal spanning tree", axes=F, pch=19, cex=0.5)
for (i in seq(nrow(idx))) {
  ft <- idx[i,]
  lines(x[ft,1], x[ft,2])
}
box()
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/","scagnosticsBase.pdf"))

