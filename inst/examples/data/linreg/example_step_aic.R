library("rio")
data("cps78_85", package="mmstat4")
xs  <- subset(cps78_85, year==85)
# Add exper^2 ?
lms <- lm (lwage~educ+exper, data=xs)
add1(lms, ~.+I(exper^2))
lms <- lm(lwage~educ+south+nonwhite+female+married+exper+union+expersq,
          data=xs)
drop1(lms) # Drop one variable
step(lms)  # Automatic backward
