library("plot.3d")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]
library("np")
# estimate Single Index Model
sim <- npindex(lwage~educ+exper, data=x)
summary(sim)
pdf("mincer_sim.pdf", width=10, height=6)
par(mfrow=c(1,2), oma=c(1,0,2,0))
new3d("s") %>% regression3d(sim, x) %>% plot3d()
new3d("c") %>% regression3d(sim, x) %>% plot3d()
if (!is.null(sim$call)) { mtext(deparse(sim$call), side=1, outer=T) }
r2 <- 1-sum(residuals(sim)^2)/sum((x$lwage-mean(x$lwage))^2)
mtext(sprintf("Single Index Model (R^2=%.3f)", r2), side=3, outer=T, cex=1.5)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/mincer_sim.pdf"))
