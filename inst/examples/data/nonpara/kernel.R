uni <- function(x) {
  return((abs(x)<1)/2)
}

epa <- function(x) {
  return((abs(x)<1)*0.75*(1-x^2))
}

dop <- T
set.seed(0)

x <- read.csv2("cps78_85.csv")
# select only year=85
x <- x[x$year==85,]

library("np")
xe  <- sort(as.vector(x$exper))
n   <- length(xe)
jt  <- jitter(rep(-0.005, n), amount=0.005)
                        
if (dop) { pdf("kernel.pdf", width=10, height=5) }
par(mfrow=c(2,3))
x  <- seq(-3,3, length.out=501)
ylim <- c(0, max(epa(x)))
plot(x, dnorm(x), type="l", main="Gaussian", ylim=ylim)
plot(x, epa(x), type="l", main="Epanechnikov", ylim=ylim)
plot(x, uni(x), type="l", main="Uniform", ylim=ylim)
            
drot <- npudens(xe, bwmethod="normal-reference")
depa <- npudens(xe, bws=drot$bw, ckertype="epa")
duni <- npudens(xe, bws=drot$bw, ckertype="uni")


plot(xe, jt, ylim=c(-0.01, max(fitted(drot))), cex=0.3, xlab="Experience", ylab="Estimated density", main=sprintf("KDE with h=%.3f", drot$bw))
lines(xe, fitted(drot), lwd=2)            
plot(xe, jt, ylim=c(-0.01, max(fitted(drot))), cex=0.3, xlab="Experience", ylab="Estimated density", main=sprintf("KDE with h=%.3f", drot$bw))
lines(xe, fitted(depa), lwd=2)            
plot(xe, jt, ylim=c(-0.01, max(fitted(drot))), cex=0.3, xlab="Experience", ylab="Estimated density", main=sprintf("KDE with h=%.3f", drot$bw))
lines(xe, fitted(duni), lwd=2)            
            
if (dop) { dev.off() }
            