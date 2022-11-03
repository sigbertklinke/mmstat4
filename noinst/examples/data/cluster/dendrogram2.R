library("cluster")
hc <- hclust(dist(USArrests))

pdf("dendrogram2.pdf", width=10, height=6)
par(mfrow=c(1,2))
ncl <- 10
plot(hc, hang=-1, main="Cluster Dendrogram with hang=-1")
par(mar=c(6,4,3.75,2))
plot(1:ncl, rev(hc$height)[1:ncl], type="b", xlab="Number of clusters", ylab="Height", ylim=c(-110,300))
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/dendrogram2.pdf"))
