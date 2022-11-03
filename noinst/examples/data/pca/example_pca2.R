library("rio")
data("bank2", package="mmstat4")
par(mfrow=c(1,2))
# do PCA (correlation)
pc <- prcomp(bank2, center=T, scale=T)
summary(pc)
plot(pc$sdev^2, type="b", main="Scree plot")
#
library("psych")
ppc<-principal(bank2, rotate="none")
ppc
scree(bank2)
