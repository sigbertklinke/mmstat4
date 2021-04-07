setwd('/home/sigbert/sync/unison/mmstat_en/shiny')

# cross sectional data
x <- read.csv('ALLBUS12.txt')
saveRDS(x, 'ALLBUS12.rds')
x <- read.csv('BOSTONHOUSING.txt')
saveRDS(x, 'BOSTONHOUSING.rds')
x <- read.csv('CARS.txt')
saveRDS(x, 'CARS.rds')
x <- read.csv('CREDIT.txt')
saveRDS(x, 'CREDIT.rds')
x <- read.csv('DECATHLON.txt')
saveRDS(x, 'DECATHLON.rds')
x <- read.csv('USCRIME.txt')
saveRDS(x, 'USCRIME.rds')

# time series
x <- read.csv2('TELEPHONE.csv', header=F)
xt <- matrix(x[,2], ncol=1)
colnames(xt) <- 'TELEPHONES.US'
ts <- ts(xt, start=c(min(x[,1]), 1))
saveRDS(ts, 'TELEPHONE.rds')

x <- read.csv2('IndexNettoKaltMietenBerlin.csv', header=F)
colnames(x) <- 'INDEX.BASIC.RENT.BERLIN'
ts <- ts(x, start=c(2005, 1), frequency=12)
saveRDS(ts, 'INDEX-BASIC-RENT-BERLIN.rds')
         