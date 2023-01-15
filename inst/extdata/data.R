setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#
library("rio")
x <- import("GSS.SAV")
vars <- c("age", "sex", "educ", "sibs", "life", "speduc", "paeduc", "maeduc", "tvhours", "wrkstat",
          "hrs1", "incomdol", "rincmdol", "wifeduc", "husbeduc", "wifeft", "happy", "hapmar")
gss <- x[,vars]
save(gss, file = "../data/gss.rda", version=2)
