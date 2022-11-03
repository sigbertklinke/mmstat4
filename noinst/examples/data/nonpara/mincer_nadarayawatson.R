library("plot.3d")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]
# estimate bivariate Nadaraya Watson 2
library("np")
nw2 <- npreg(lwage~educ+exper, data=x)
summary(nw2)
pdf ("mincer_nadarayawatson.pdf", width=10, height=6)
par(mfrow=c(1,2), oma=c(1,0,2,0))
new3d("s") %>% regression3d(nw2, x) %>% plot3d()
new3d("c") %>% regression3d(nw2, x) %>% plot3d()
if (!is.null(nw2$call)) { mtext(deparse(nw2$call), side=1, outer=T) }
r2 <- 1-sum(residuals(nw2)^2)/sum((x$lwage-mean(x$lwage))^2)
mtext(sprintf("Bivariate Kernel Regression (R^2=%.3f)", r2), side=3, outer=T, cex=1.5)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/mincer_nadarayawatson.pdf"))
