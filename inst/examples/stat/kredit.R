setwd("~/sync/unison/mmstat_en/shiny")

xo <- read.table('kredit.asc', header=T)

x <- readRDS('CREDIT.rds')
x$CREDITWORTHY <- factor(x$CREDITWORTHY, labels=c("not credit-worthy", "credit-worthy"))
table(x$CREDITWORTHY)

x$ACCOUNT <- factor(x$BALANCE!=1, labels=c("no", "yes"))
prop.table(table(x$CREDITWORTHY, x$ACCOUNT), 1)
x$BALANCE <- NULL

x$REPAYMENT <- factor(x$REPAYMENT!=1, labels=c("problems", "no problems"))
prop.table(table(x$CREDITWORTHY, x$REPAYMENT), 1)

x$PURPOSE <- factor(xo$verw, labels=c("other", "new car", "used car", "furniture", "television", "household appliances", "repair", "vacation", "retraining", "business"))
prop.table(table(x$CREDITWORTHY, x$PURPOSE), 1)

x$SAVINGS <- factor(xo$sparkont==1, labels=c("yes", "no")) 
prop.table(table(x$CREDITWORTHY, x$SAVINGS), 1)

x$EMPLOYED_SINCE <- factor(x$EMPLOYED_SINCE, labels=c("more than one year", "less than one year")) 
prop.table(table(x$CREDITWORTHY, x$EMPLOYED_SINCE), 1)

x$FAMILY_GENDER <- factor(x$FAMILY_GENDER, labels=c("other", "male divorced/living apart")) 
prop.table(table(x$CREDITWORTHY, x$FAMILY_GENDER), 1)

x$GUARANTOR <- factor(x$GUARANTOR, labels=c("yes", "no")) 
prop.table(table(x$CREDITWORTHY, x$GUARANTOR), 1)

tmp <- x$AT_PRES_ADDR
x$AT_PRES_ADDR <- factor(x$LIVING, labels=c("<1 year", "1-4 years", "4-7 years", ">=7 years"))
prop.table(table(x$CREDITWORTHY, x$AT_PRES_ADDR), 1)

x$LIVING <- factor(tmp, labels=c("house/land", "other/none"))
prop.table(table(x$CREDITWORTHY, x$LIVING), 1)

x$OTHERCREDITS <- factor(x$OTHERCREDITS, labels=c("no", "yes")) 
prop.table(table(x$CREDITWORTHY, x$OTHERCREDITS), 1)

x$APPARTMENT_TYPE <- factor(x$APPARTMENT_TYPE, labels=c("other", "rented")) 
prop.table(table(x$CREDITWORTHY, x$APPARTMENT_TYPE), 1)

x$PREV_CREDIT <- factor(x$PREV_CREDIT, labels=c("1 or more", "none")) 
prop.table(table(x$CREDITWORTHY, x$PREV_CREDIT), 1)

x$OCCUPATION<- factor(xo$beruf<3, labels=c("skilled", "unskilled/umemployed")) 
prop.table(table(x$CREDITWORTHY, x$OCCUPATION), 1)

x$MAINTENANCE<- factor(x$MAINTENANCE, labels=c("3 and more", "0 to 2")) 
prop.table(table(x$CREDITWORTHY, x$MAINTENANCE), 1)

x$TELEPHONE<- factor(x$TELEPHONE, labels=c("no", "yes")) 
prop.table(table(x$CREDITWORTHY, x$TELEPHONE), 1)

x$FOREIGN_WORKER<- factor(x$FOREIGN_WORKER, labels=c("yes", "no")) 
prop.table(table(x$CREDITWORTHY, x$FOREIGN_WORKER), 1)

saveRDS(x, 'CREDIT.rds')

x <- readRDS('CREDIT.rds')
for (name in names(x)) {
  if (is.factor(x[[name]])) {
    print(name)
    xn <- as.numeric(x[[name]])
    print(table(xn, x[[name]]))
  }
}
