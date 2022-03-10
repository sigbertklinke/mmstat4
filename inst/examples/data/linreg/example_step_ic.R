library("rio")
data("cps78_85", package="mmstat4")
xs  <- subset(cps78_85, year==85)
library("olsrr")
# Largest model: all vars must be explicitly written
llm <- lm (lwage~educ+exper+I(exper^2), data=xs)
ols_step_forward_aic(llm)
ols_step_backwardward_aic(llm)
ols_step_both_aic(llm)
