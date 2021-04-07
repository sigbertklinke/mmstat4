library("MASS")  # for Boston Housing data
library("np")
bw <- npregbw(medv~lstat+rm, data=Boston)
mhat <- npreg(bw)
plot(mhat, view="fixed", phi=30, theta=75)
plot(Boston$lstat, Boston$rm, pch=19, cex=0.5)