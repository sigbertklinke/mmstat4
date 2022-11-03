library("QuantPsyc")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]

pdf("mincer_exploratory.pdf", width=10, height=6)
par(mfrow=c(1,2))
plot(x$educ, x$lwage, pch=19, cex=0.5, xlab="Education (in years)", ylab="log(hourly wage)")
lm1 <- lm(lwage~educ, data=x)
abline(lm1, col="red", lwd=2)
plot(x$exper, x$lwage, pch=19, cex=0.5, xlab="Experience (in years)",  ylab="log(hourly wage)")
lm1 <- lm(lwage~exper, data=x)
abline(lm1, col="red", lwd=2)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/mincer_exploratory.pdf"))
