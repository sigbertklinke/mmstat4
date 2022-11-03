data("fatalities_statlib", package="mmstat4")
# see http://lib.stat.cmu.edu/DASL/Stories/hwfatal.html
lm <- lm (US~YR, data=fatalities_statlib)
summary(lm)
plot(fatalities_statlib$YR, fatalities_statlib$US)
abline(lm, col="red")
#
library("car")
durbinWatsonTest(lm)
#
library("lmtest")
dwtest(lm)
dwtest(lm, alternative="two.sided")
