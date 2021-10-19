data("cps78_85", package="mmstat4")
# select only year=85
x <- x[x$year==85,]
library("rpart")
fullfit <- rpart(lwage~educ+exper, data=x, method="anova", control=rpart.control(cp=0))
prunefit<-prune(fullfit, cp=fullfit$cptable[which.min(fullfit$cptable[,"xerror"]),"CP"])

pdf("RCART4d.pdf", width=10, height=5)
plot(prunefit, main="Pruned CART for CPS85 data (R^2=0,252)")
text(prunefit, use.n=TRUE, all=TRUE, cex=.8)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/RCART4d.pdf"))