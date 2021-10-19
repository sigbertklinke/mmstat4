library(lattice)
# trellis1.pdf
pdf("trellis1.pdf", bg="transparent")
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), stack = TRUE,
         auto.key = list(space = "right"),
         ylab = "Barley Yield (bushels/acre)",
         scales = list(x = list(rot = 45)))
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/trellis1.pdf"))