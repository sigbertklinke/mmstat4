data("allbus2012", package="mmstat4")
body        <- as.data.frame(allbus2012)
names(body) <- c("age", "height", "weight")
# number of NAs
nabody <- is.na(body)
apply(nabody, 2, sum)
# full data
mean(body$weight)
cor(body)
# case deletion
mean(body$weight, na.rm=T)
cor(body, use="complete.obs")
sum(complete.cases(body))
# available case analysis
cor(body, use="pairwise.complete.obs")
crossprod(!nabody)