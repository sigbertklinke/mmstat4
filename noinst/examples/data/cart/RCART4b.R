data("cps78_85", package="mmstat4")
# select only year=85
x <- x[x$year==85,]
lrfit <- lm (lwage~educ+exper, data=x)
summary(lrfit)

library("rpart")
fullfit <- rpart(lwage~educ+exper, data=x, method="anova", control=rpart.control(cp=0))
pdf("RCART4b.pdf", width=10, height=5)
plot(fullfit, main="Full CART for CPS85 data (R^2=0,320)")
text(fullfit, cex=.8)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/RCART4b.pdf"))
