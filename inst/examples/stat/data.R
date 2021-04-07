###CREDIT
x <- read.table('/home/sigbert/sync/unison/mmstat_en/shiny/kredit.asc', header=T)

df <- data.frame(CREDITWORTHY    = x$kredit,
                 BALANCE         = x$laufkont,
                 DURATION        = x$laufzeit,
                 REPAYMENT       = x$moral,
                 PURPOSE         = x$verw,
                 AMOUNT          = x$hoehe,
                 SAVINGS         = x$sparkont,
                 EMPLOYED_SINCE = x$beszeit,
                 FAMILY_GENDER   = x$famges,
                 GUARANTOR       = x$buerge,
                 AT_PRES_ADDR    = x$wohnzeit,
                 ASSETS          = x$verm,
                 AGE             = x$alter,
                 OTHERCREDITS    = x$weitkred,
                 APPARTMENT_TYPE = x$wohn,
                 PREV_CREDIT     = x$bishkred,
                 OCCUPATION      = x$beruf,
                 MAINTENANCE     = x$pers,
                 TELEPHONE       = x$telef,
                 FOREIGN_WORKER  = x$gastarb)

df <- data.frame(CREDITWORTHY    = x$kredit==1,
                 BALANCE         = x$laufkont<2,
                 DURATION        = x$laufzeit,
                 REPAYMENT       = x$moral<2,
                 PURPOSE         = (x$verw==1)||(x$verw==3)||(x$verw==8),
                 AMOUNT          = x$hoehe,
                 SAVINGS         = x$sparkont<1,
                 EMPLOYED_SINCE  = x$beszeit<3,
                 FAMILY_GENDER   = x$famges<2,
                 GUARANTOR       = x$buerge!=2,
                 LIVING          = x$wohnzeit,
                 AT_PRES_ADDR    = x$verm<4,
                 AGE             = x$alter,
                 OTHERCREDITS    = x$weitkred<3,
                 APPARTMENT_TYPE = x$wohn==2,
                 PREV_CREDIT     = x$bishkred<2,
                 OCCUPATION      = (x$beruf==2)||(x$beruf==3),
                 MAINTENANCE     = x$pers==1,
                 TELEPHONE       = x$telef==2,
                 FOREIGN_WORKER  = x$gastarb==1)
df <-lapply(df, function(x) { if(is.logical(x)) ifelse(x,1,0) else x })
write.csv(df, '/home/sigbert/sync/unison/mmstat_en/shiny/CREDIT.txt', row.names=F)


check <- function (var) {
  print(prop.table(table(var)))
  tab <- table(x$kredit, var)
  tab2p <- prop.table(tab, 2)
  print(tab2p)
  d <- tab2p[2,]
  dist <- as.dist(outer(d, d, function(x,y) (x-y)^2))
  hc <- hclust(dist)
  plot(hc)
}

check(x$kredit)
check(x$laufkont)
check(x$moral)
check(x$verw)
check(x$sparkont)
check(x$beszeit)
check(x$famges)
check(x$buerge)
check(x$wohnzeit)
check(x$verm)
check(x$weitkred)
check(x$wohn)
check(x$bishkred)
check(x$beruf)
check(x$pers)
check(x$telef)
check(x$gastarb)

###BOSTONHOUSING
library("MASS")
names(Boston) <- toupper(names(Boston))
write.csv(Boston, '/home/sigbert/sync/unison/mmstat_en/shiny/BOSTONHOUSING.txt', row.names=F)

