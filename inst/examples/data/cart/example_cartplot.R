library("rpart")
library("rpart.plot")
library("MASS")  # for Boston Housing data
model <- rpart(medv~lstat+rm, data=Boston)
rpart.plot(model)