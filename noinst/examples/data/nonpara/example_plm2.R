library("MASS")  # for Boston Housing data
library("mgcv")
library("plot.3d")
model <- gam(medv~s(lstat)+rm, data=Boston)
new3d("c") %>% regression3d(model, Boston) %>% plot3d()
plot(model)