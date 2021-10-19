data("cps78_85", package="mmstat4")
# select only year=85
x <- x[x$year==85,]
lrfit <- lm (lwage~educ+exper, data=x)
summary(lrfit)

library("rpart")
fullfit <- rpart(lwage~educ+exper, data=x, method="anova", control=rpart.control(cp=0))
pdf("RCART4c.pdf", width=10, height=5)
plotcp(fullfit)
prunefit<-prune(fullfit, cp=fullfit$cptable[which.min(fullfit$cptable[,"xerror"]),"CP"])
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/RCART4c.pdf"))

