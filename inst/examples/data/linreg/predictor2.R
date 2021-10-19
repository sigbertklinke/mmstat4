library("QuantPsyc")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]

pdf("predictor2.pdf", width=10, height=5)
plot(x$educ, type="b", main="Sequence plot of education", ylab="Education (in years)", pch=19)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/predictor2.pdf"))

