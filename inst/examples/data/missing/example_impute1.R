library("rio")
x <- import("https://shinyapps.wiwi.hu-berlin.de/d/ALLBUS2012.SAV")
body <- as.data.frame(x[,c(220,593,595)])
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