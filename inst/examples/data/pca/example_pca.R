library("rio")
data("bank2", package="mmstat4")
# do PCA (covariance)
pc <- prcomp(bank2)
pc
# what R delivers
summary(pc)
par(mfrow=c(1,2))
plot(pc, main="Scree plot as bar chart")
plot(pc$sdev^2, type="b", main="Scree plot")
