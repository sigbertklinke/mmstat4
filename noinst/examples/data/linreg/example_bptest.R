library("rio")
data("staedtemietenr", package="mmstat4")
x  <- staedtemietenr[complete.cases(staedtemietenr),]
lm <- lm (Miete~Fläche, data=x)
summary(lm)
plot(x$Fläche, residuals(lm))
abline(h=0, col="red")
#
library("lmtest")
bptest(Miete~Fläche, data=x)
