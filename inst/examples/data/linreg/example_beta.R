library("rio")
x  <- import("https://shinyapps.wiwi.hu-berlin.de/d/CPS78-85.tsv")
xs  <- subset(x, year==85, c("lwage", "educ"))
lms <- lm (scale(lwage)~scale(educ), data=xs)
summary(lms)
#
library("QuantPsyc")
lm <- lm (lwage~educ, data=x, subset=(year==85))
lm.beta(lm)