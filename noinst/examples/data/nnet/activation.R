t <- (-300:300)/100
pdf("activation.pdf")
plot(t, 0.5+0.5*tanh(t), type="l", ylab=expression(Lambda(t)), main="Activation function", col="green", lwd=2)
lines(t, 0.5+0.5*sign(t), col="black", lwd=2)
lines(t, 1/(1+exp(-t)), col="red", lwd=2)
lines(t, sqrt(2*pi)*dnorm(t), col="blue", lwd=2)
legend("topleft", legend=c("Step", "Sigmoid", "Tanh", "Radial basis"), col=c("black", "red", "green", "blue"), lwd=2)
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/activation.pdf"))
