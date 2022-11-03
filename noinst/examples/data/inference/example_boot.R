library("boot")
meanboot <- function (x, ind) { return(mean(x[ind])); }
#
set.seed(24961970)
data("pechstein", package="mmstat4")
boot(pechstein$Retikulozyten, meanboot, 999)
