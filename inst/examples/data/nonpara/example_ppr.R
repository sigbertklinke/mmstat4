library("MASS")  # for Boston Housing data
library("plot.3d")
model <- ppr(medv~lstat+rm, data=Boston, nterm=2)
new3d("c") %>% regression3d(model, Boston) %>% plot3d()
par(mfrow=c(1,2))
plot(model)