setwd("~/unison/mmstat_en/shiny")
library("foreign")
xn <- read.spss("/home/sk/unison/Daten/ALLBUS/ALLBUS80-12.sav", to.data.frame=, use.value.labels=F)
x <- read.spss("/home/sk/unison/Daten/ALLBUS/ALLBUS80-12.sav", to.data.frame=T)

k  <- rep(NA, length(x)) 
for (i in seq(ncol(x))) k[i]  <- length(unique(x[,i]))
kn <- attr(x, 'variable.labels')
names(k) <- paste(names(kn), kn)
k <- k[colSum(is.na(x))<0.2*ncol(x)]
sort(k)

allbus <- data.frame(YEAR=xn$V2, 
                     WEST=x$V5, INTERVIEW=x$V7, SEX=x$V653, MEMBER.TRADE.UNION=x$V1639, GERMAN.SINCE.BIRTH=x$V1189,
                     SUPPORT.POLITIC.PARTY=x$V27, ELIGIBLE.VOTE.LAST.FEDERAL.ELECTION=x$V31, 
                     AGE=x$V651, BODY.MASS.INDEX=x$V474, BODY.HEIGHT=x$V470, BODY.WEIGHT=x$V472, 
                     RESP.MONTHLY.INCOME=x$V813, HH.NETINCOME=x$V820)
head(allbus)
tab<-apply(allbus, 2, function (x) { tapply(x, allbus$YEAR, function (xx) { sum(!is.na(xx)) })})
apply(tab>100, 1, sum)

years <- c(2002, 2004, 2012)
for (year in years) {
  allbusyear <- allbus[allbus$YEAR==year,]
  dropvar    <- apply(allbusyear, 2, function(x) { sum(!is.na(x)) })<100
  for (i in seq(names(allbusyear))) { if(dropvar[i]) allbusyear[[names(dropvar)[i]]] <- NULL }
  allbusyear$YEAR <- NULL
  cc <- complete.cases(allbusyear)
  allbusyear <- allbusyear[cc,]
  saveRDS(allbusyear, sprintf('ALLBUS%4i-GENERAL.rds', year))
}