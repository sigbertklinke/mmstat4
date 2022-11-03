data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]
library("rpart")
fit <- rpart(lwage~educ+exper, data=x, method="anova")
pdf("RCART4a.pdf", width=10, height=5)
plot(fit, main="CART with default settings for CPS85 data (R^2=0,252)")
text(fit, cex=.8)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/RCART4a.pdf"))
