# set file a current dir
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#
library("usethis")
library("rio")

lotto <- import("mmstat/lottozahlen.csv")
use_data(lotto, overwrite = TRUE)
#
data12411 <- import("stat/12411-0006.csv", encoding="UTF-8")
use_data(data12411, overwrite = TRUE)
#
data13211 <- import("stat/13211-0002.csv", encoding="UTF-8")
for (name in names(data13211)[4:9]) {
  data13211[[name]] <- as.numeric(gsub(",", ".", data13211[[name]], fixed=TRUE))
}
use_data(data13211, overwrite = TRUE)
#
hhD <- import("stat/haushalte.csv", format="csv2")
use_data(hhD, overwrite = TRUE)
#
hhB <- import("stat/haushalte_berlin.csv", format="csv2")
use_data(hhB, overwrite = TRUE)
