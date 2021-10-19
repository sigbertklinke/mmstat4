library("rio")
data("cps78_85", package="mmstat4")
xs  <- subset(cps78_85, year==85, c("lwage", "educ"))
lms <- lm (scale(lwage)~scale(educ), data=xs)
summary(lms)
#
library("QuantPsyc")
lm <- lm (lwage~educ, data=xs)
lm.beta(lm)
