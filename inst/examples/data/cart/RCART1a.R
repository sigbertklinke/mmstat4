data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]
#
library("rpart")
library("rpart.plot")
fit<-rpart(lwage~educ+exper, data=x)
pdf("RCART1a.pdf", width=5, height=10)
rpart.plot(fit)
text(fit, use.n=TRUE, all=TRUE, cex=0.8)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/RCART1a.pdf"))
