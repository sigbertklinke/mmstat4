data("bostonh", package="mmstat4")
bhd   <- bostonh[,-c(4,9)]
vars  <- names(bhd)[-12]
lmi   <- lm(MEDV~1, data=bostonh)
best  <- c(r2=summary(lmi)$r.squared, r2adj=summary(lmi)$adj.r.squared,
           aic=AIC(lmi), bic=BIC(lmi))
model <- list(r2="1", r2adj="1", aic="1", bic="1")

for (i in ((1:2^12)-1)) {
  s   <- as.logical(intToBits(i))[1:12]
  fm  <- paste0(vars[s[-1]], collapse="+")
  fm  <- paste0("MEDV~", fm, "+1")
  lmi <- lm(as.formula(fm), data=bhd)
  if (summary(lmi)$r.square>best["r2"]) {
    best["r2"] <- summary(lmi)$r.square
    model$r2 <- fm
  }
  if (summary(lmi)$adj.r.square>best["r2adj"]) {
    best["r2adj"] <- summary(lmi)$adj.r.square
    model$r2adj <- fm
  }
  if (AIC(lmi) < best["aic"]) {
    best["aic"] <- AIC(lmi)
    model$aic<- fm
  }
  if (BIC(lmi) < best["bic"]) {
    best["bic"] <- BIC(lmi)
    model$bic<- fm
  }
}
