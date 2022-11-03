library("rio")
data("bank2", package="mmstat4")
par(mfrow=c(1,2))
# do parallel analysis
library("paran")
paran(bank2, centile=95, all=T, graph=T)
# adjusted ev = unadjusted - bias (random ev-1)
library("psych")
fa.parallel(bank2, fa="pc")
