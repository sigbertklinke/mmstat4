BEGIN PROGRAM R.
data <- spssdata.GetDataFromSPSS(factorMode="labels")
#
#install.packages("tabplot")
library("tabplot")
tableplot(data)
END PROGRAM.
