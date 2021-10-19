library("QuantPsyc")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]

lm2 <- lm(lwage~educ+exper, data=x)
summary(lm2)
lm.beta(lm2)
pdf("mincer_residuals.pdf", width=10, height=6)
par(mfrow=c(2,2))
plot(lm2, pch=19, cex=0.5, lwd=2)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/mincer_residuals.pdf"))
