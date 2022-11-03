library(lattice)
# trellis3.pdf
pdf("trellis3.pdf", bg="transparent")
Depth <- equal.count(quakes$depth, number=8, overlap=.1)
xyplot(lat ~ long | Depth, data = quakes)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/trellis3.pdf"))