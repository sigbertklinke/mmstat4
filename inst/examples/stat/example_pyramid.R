# install.packages("plotrix")
# install.packages("rio")
library("rio")
library("plotrix")
#
burl <- "https://box.hu-berlin.de/d/78416cc0ba604b9cbf66/files/"
x <- import(paste0(burl, "?p=/stat/12411-0006.csv&dl=1"),
            format="csv")
pyramid.plot(x$M/1e5, x$W/1e5,
             labels=c(1:85, ">85"), labelcex=0.65,
             lxcol="blue", rxcol="red", unit="in 100000",
             top.labels=c("Maennlich", "Alter", "Weiblich")
             )
