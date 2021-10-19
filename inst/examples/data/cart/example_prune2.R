data("cps78_85", package="mmstat4")
x <- cps78_85[cps78_85$year==85,]
library("rpart")
fullfit  <- rpart(lwage~educ+exper, data=x, method="anova", control=rpart.control(cp=0))
prunefit <- prune(fullfit, cp=fullfit$cptable[which.min(fullfit$cptable[,"xerror"]),"CP"])
