library("MASS")  # for Boston Housing data
library("mgcv")
library("plot.3d")
model <- gam(medv~s(lstat)+s(rm), data=Boston)
new3d("s") %>% regression3d(model, Boston) %>% plot3d()
par(mfrow=c(1,2))
plot(model)