library("cluster")
d  <- dist(iris[,1:4])
hc <- hclust(d)

pdf("iris_pairs.pdf")
pairs(iris[1:4],main="Iris Data (red=setosa,green=versicolor,blue=virginica)", pch=21,
      bg=c("red","green3","blue")[unclass(iris$Species)])
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/iris_pairs.pdf"))
