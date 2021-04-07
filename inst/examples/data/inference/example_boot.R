library("rio")
library("boot")
meanboot <- function (x, ind) { return(mean(x[ind])); }
#
set.seed(24961970)
x <- import("https://shinyapps.wiwi.hu-berlin.de/d/pechstein.sav")
head(x)
boot(x$RETIKULOZYTEN[1:29], meanboot, 999)
