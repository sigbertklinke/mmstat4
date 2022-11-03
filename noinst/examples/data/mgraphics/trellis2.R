library(lattice)
# trellis2.pdf
pdf("trellis2.pdf", bg="transparent")
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free", layout = c(2, 2),
       auto.key = list(x = .6, y = .7, corner = c(0, 0)))
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/trellis2.pdf"))