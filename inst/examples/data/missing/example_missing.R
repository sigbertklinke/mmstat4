library("rio")
x <- import("https://shinyapps.wiwi.hu-berlin.de/d/ALLBUS2012.SAV")
trust <- as.data.frame(x[,52:63])
# count number of missing values
r <- is.na(trust)
# no. per column
apply(r, 2, sum)
# no. per row
apply(r, 1, sum)
# number of complete cases
cc <- complete.cases(trust)
sum(cc)
# filter functions
head(na.omit(trust))
try(na.fail(trust))