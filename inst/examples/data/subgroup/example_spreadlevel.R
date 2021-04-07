data(Boston, package="MASS")
library("car")
spreadLevelPlot(Boston$medv, by=Boston$rad)