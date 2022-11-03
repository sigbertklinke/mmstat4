zfaithful <- apply(faithful, 2, scale)
# k-median
library("flexclust")
cl1 <- kcca(zfaithful, 2, family=kccaFamily('kmedians'))
plot(zfaithful, col=cl1@second)
cl1@centers