library("mmstat4")
library("plotrix")
data(data12411)
pyramid.plot(data12411$M/1e5, data12411$W/1e5,
             labels=c(1:85, ">85"), labelcex=0.65,
             lxcol="blue", rxcol="red", unit="in 100000",
             top.labels=c("Maennlich", "Alter", "Weiblich")
             )
