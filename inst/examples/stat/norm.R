n <- 1000
mu <- c(0,1,5,10)
sd <- c(1,2,5)
x<-matrix(NA, nrow=n, ncol=length(mu)*length(sd))
k <- 1
xn <- c()
for (i in seq(mu)) {
  for (j in seq(sd)) {
    x[,k]<-rnorm(n, mean=mu[i], sd=sd[j])
    xn <- c(xn, sprintf("NORM_%i_%i", mu[i], sd[j]))
    k <- k+1
  }
}
colnames(x) <-xn
df <- as.data.frame(x)
saveRDS(df, "~/sync/unison/mmstat_en/shiny/NORM.rds")
