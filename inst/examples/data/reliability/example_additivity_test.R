#install.packages("devtools")
#library("devtools")
#install_github("simecek/additivityTests")
library("psych")
# extract first factor
bfi2 <- na.omit(bfi)
fa   <- fa(bfi2)
vars <- (abs(fa$loadings)>0.5)
# create corrected items matrix
keys  <- keys[vars*sign(fa$loadings)]
items <- reverse.code(keys, bfi2[,vars])
# additivity test
library("additivityTests")
tukey.test(items)