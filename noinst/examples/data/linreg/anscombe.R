library("lattice")
data("anscombe", package="mmstat4")
pdf("anscombe.pdf", bg="transparent")
xyplot(Y~X|ID, data=anscombe, main="Anscombe quartet", panel=function(x, y){
  panel.xyplot(x, y)
  panel.lmline(x, y)
})
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/anscombe.pdf"))
