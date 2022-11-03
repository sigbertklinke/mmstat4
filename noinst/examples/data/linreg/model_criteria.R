data("bostonh", package="mmstat4")
bhd  <- bostonh[,-c(4,9)]
vars <- names(bhd)[-12]
lmi  <- lm(MEDV~1, data=bostonh)
crit <- list(r2=summary(lmi)$r.squared,
             r2adj=summary(lmi)$adj.r.squared,
             aic=AIC(lmi), bic=BIC(lmi))
model <- "1"
for (i in seq(vars)) {
  res   <- add1(lmi, as.formula(paste0("MEDV~", paste0(vars, collapse="+"))))
  model <- c(model, row.names(res)[which.min(res$RSS)])
  lmi <- lm(as.formula(paste0("MEDV~", paste0(model, collapse="+"))), data=bostonh)
  crit$r2    <- c(crit$r2, summary(lmi)$r.squared)
  crit$r2adj <- c(crit$r2adj, summary(lmi)$adj.r.squared)
  crit$aic   <- c(crit$aic, AIC(lmi))
  crit$bic   <- c(crit$bic, BIC(lmi))
}
pdf("model_criteria.pdf", width=10)
par(mfrow=c(2,2))
plot(crit$r2, type="b", axes=FALSE, main="R^2", xlab="", ylab="")
abline(v=which.max(crit$r2))
axis(1, at=1:12, labels=model, las=2)
axis(2)
box()
plot(crit$r2adj, type="b", axes=FALSE, main="Adjusted R^2", xlab="", ylab="")
abline(v=which.max(crit$r2adj))
axis(1, at=1:12, labels=model, las=2)
axis(2)
box()
plot(crit$aic, type="b", axes=FALSE, main="AIC", xlab="", ylab="")
abline(v=which.min(crit$aic))
axis(1, at=1:12, labels=model, las=2)
axis(2)
box()
plot(crit$bic, type="b", axes=FALSE, main="BIC", xlab="", ylab="")
abline(v=which.min(crit$bic))
axis(1, at=1:12, labels=model, las=2)
axis(2)
box()
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/model_criteria.pdf"))

