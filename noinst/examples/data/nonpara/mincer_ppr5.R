library("plot.3d")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]
# estimate Projection Pursuit Regression
ppr5 <- ppr(lwage~educ+exper, data=x, nterm=5)
summary(ppr5)
pdf("mincer_ppr5.pdf", width=10, height=6)
par(mfrow=c(1,2), oma=c(1,0,2,0))
new3d("s") %>% regression3d(ppr5, x) %>% plot3d()
new3d("c") %>% regression3d(ppr5, x) %>% plot3d()
if (!is.null(ppr5$call)) { mtext(deparse(ppr5$call), side=1, outer=T) }
r2 <- 1-sum(residuals(ppr5)^2)/sum((x$lwage-mean(x$lwage))^2)
mtext(sprintf("Projection Pursuit Regression Model (R^2=%.3f)", r2), side=3, outer=T, cex=1.5)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/mincer_ppr5.pdf"))
