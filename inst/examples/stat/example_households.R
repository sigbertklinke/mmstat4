library("rio")
burl <- "https://box.hu-berlin.de/d/78416cc0ba604b9cbf66/files/"
x <- import(paste0(burl, "?p=/stat/haushalte.csv&dl=1"),
            format="csv2")
#
ylim <- c(0, max(x$Einpersonen, na.rm=T))
plot(x$Jahr, x$Einpersonen, xlab="", ylab="Anteil (in %)",
     main="Anteil x Personen Haushalte",
     sub="Bis 1990 Frueheres Bundesgebiet",
     pch=19, cex=0.75, ylim=ylim)
#
points(x$Jahr, x$Zweipersonen, pch=19, cex=0.75, col="red")
points(x$Jahr, x$Dreipersonen, pch=19, cex=0.75, col="blue")
points(x$Jahr, x$Vierpersonen, pch=19, cex=0.75, col="green")
points(x$Jahr, x$'Fuenf und mehr Personen',
       pch=19, cex=0.75, col="orange")
#
legend("topleft", title="Haushalte mit", pch=19, cex=0.75,
       col=c("black", "red", "blue", "green", "orange"),
       legend=c("1 Person", "2 Personen", "3 Personen",
                "4 Personen", "5+ Personen"))

