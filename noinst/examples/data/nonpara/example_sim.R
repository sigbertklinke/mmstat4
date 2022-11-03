library("MASS")  # for Boston Housing data
library("np")
model <- npindex(medv~lstat+rm, data=Boston)
library("plot.3d")
new3d("s") %>% regression3d(model, Boston) %>% plot3d()
plot(model)
