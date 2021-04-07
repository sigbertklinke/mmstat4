zfaithful <- apply(faithful, 2, scale)
# k-medoid
library("cluster")
cl2 <- pam(zfaithful, 2)
plot(zfaithful, col=cl2$clustering)
cl2$medoids