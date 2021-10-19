library("QuantPsyc")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]

pdf("predictor1.pdf", width=10, height=3)
par(mfrow=c(1,2))
stripchart(x$educ, method="jitter", jitter=1, ylim=c(1,2),  main="Strip plot of education", xlab="Education (in years)")
boxplot(x$educ, horizontal=T,  main="Boxplot of education", xlab="Education (in years)")
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/predictor2.pdf"))
