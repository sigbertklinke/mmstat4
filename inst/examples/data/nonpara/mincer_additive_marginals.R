library("scatterplot3d")
library("locfit")
library("np")
library("gam")
data("cps78_85", package="mmstat4")
# select only year=85
x <- cps78_85[cps78_85$year==85,]

# estimate Additive Model
am <- gam(lwage~s(educ)+s(exper), data=x)
pdf("mincer_additive_marginals.pdf", width=10, height=6) 
par(mfrow=c(1,2))
plot(am)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/mincer_additive_marginals.pdf"))