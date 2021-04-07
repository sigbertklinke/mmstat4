regime <- function (...) {
  args <- list(...)
  nr   <- length(args[[1]])
  ret  <- matrix(NA, nrow=nr, ncol=0)
  for (i in seq(1, length(args), by=2)) {
    ret <- cbind(ret, matrix(args[[i]], nrow=nr, ncol=args[[i+1]]))
  }
  ret
}
#
nprob   <- 0:10
drosten <- c(0, 0.9, 0, 0, 0, 0, 0, 0, 0, 0, 0.1) 
sars1   <- c(0.73, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.03, 0.02, 0.02, 0.02) # SARS-1
r09     <- c(0.3, 0.5, 0.2, 0, 0, 0, 0, 0, 0, 0, 0)
uni     <- rep(1/11, 11)
par(mfrow=c(2,2))
plot(nprob, drosten, main="Drosten", type="h", sub=sprintf("R=%.2f, K=%.2f",  sum(drosten*nprob), 1-sum(drosten^2)))
plot(nprob, sars1, main="SARS-1", type="h", sub=sprintf("R=%.2f, K=%.2f",  sum(sars1*nprob), 1-sum(sars1^2)))
plot(nprob, uni, main="Uniform", type="h", sub=sprintf("R=%.2f, K=%.2f",  sum(uni*nprob), 1-sum(uni^2)))
plot(nprob, r09, main="R=0.9", type="h", sub=sprintf("R=%.2f, K=%.2f",  sum(r09*nprob), 1-sum(r09^2)))
#
regime  <- regime(drosten, 15)
#
T <- ncol(regime)
infected    <- rep(NA, T)
newinfected <- rep(NA, T)
R           <- rep(NA, T)
r           <- rep(sum(nprob*regime[,1]), T)
c           <- rep(1-sum(regime[,1]^2), T)
infected[1] <- newinfected[1] <- 10
for (t in 2:T) {
  prob <- regime[,t]
  r[t]    <- sum(prob*nprob)
  c[t]    <- 1-sum(prob^2)
  if (newinfected[t-1]>10000) {
    newinfected[t] <- newinfected[t-1]*r[t]
  } else {
    newinf <- sample(nprob, size=newinfected[t-1], replace=TRUE, prob=prob)
    print(table(newinf))
    newinfected[t] <- sum(newinf)
  }
# print(table(newinfected[t]))
  infected[t] <- infected[t-1]+newinfected[t]
  R[t]        <- newinfected[t]/newinfected[t-1]
#  print(c(infected[t], newinfected[t], R[t]))
}
par(mfrow=c(2,2))
plot(1:T, log10(infected), type="b", main="Total infections")
if (any(infected>7e9)) abline(h=log10(7e9), col="blue")
plot(1:T, log10(newinfected), type="b", main="New infections")
plot(1:T, R, type="b", ylim=c(0, max(R[is.finite(R)], r)), main="R")
lines(1:T, r, col="red")
plot(1:T, c, type="b", ylim=c(0,1), main="K")