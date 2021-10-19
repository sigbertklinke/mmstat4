library("cluster")
d  <- dist(iris[,1:4])
hc <- hclust(d)

png("iris_pairs.png", height=1000, width=1000)
pairs(iris[1:4],main="Iris Data (red=setosa,green=versicolor,blue=virginica)", pch=21,
      bg=c("red","green3","blue")[unclass(iris$Species)])
dev.off()


png("iris_silhouette.png", height=1000, width=1000)
c2 <- cutree(hc, k=2)
c3 <- cutree(hc, k=3)
c4 <- cutree(hc, k=4)
si2 <- silhouette(c2, d)
si3 <- silhouette(c3, d)
si4 <- silhouette(c4, d)
par(mfrow=c(2,2))
plot(hc, hang = -1)
plot(si2, col="black")
plot(si3, col="black")
plot(si4, col="black")
dev.off()
