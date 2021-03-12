n <- 6
x <- 1:n
freq <- rep(1/n, n)
plot(x, freq, type="h", main="Wahrscheinlichkeitsfunktion Gleichverteilung")
points(x, freq, pch=19)
cdf <- approxfun(x, cumsum(freq), method="constant",
								 yleft=0, yright=1, f=0)
class(cdf) <- c("ecdf", "stepfun", class(cdf))
plot(cdf, main="Verteilungsfunktion  Gleichverteilung")