library("plot.3d")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]
# estimate linear model 1 (exper only linear !)
lr1 <- lm (lwage~educ+exper, data=x)
pdf("mincer_linreg.pdf", width=10, height=6)
par(mfrow=c(1,2), oma=c(1,0,2,0))
new3d("s") %>% regression3d(lr1, x) %>% plot3d()
new3d("c") %>% regression3d(lr1, x) %>% plot3d()
if (!is.null(lr1$call)) { mtext(deparse(lr1$call), side=1, outer=T) }
r2 <- 1-sum(residuals(lr1)^2)/sum((x$lwage-mean(x$lwage))^2)
mtext(sprintf("Linear Regression Model (R^2=%.3f)", r2), side=3, outer=T, cex=1.5)
dev.off() 
if (interactive()) browseURL(paste0(getwd(),"/mincer_linreg.pdf"))
