library("rio")
#
burl <- "https://box.hu-berlin.de/d/78416cc0ba604b9cbf66/files/"
x <- import(paste0(burl, "?p=/stat/13211-0002.csv&dl=1"),
            format="csv")
#
ts <- ts(x$Arbeitslose/1e6, start=c(2005,1), frequency=12)
plot(ts, main="Arbeitslose in Deutschland (in Mio)", xlab="")
#
ts <- ts(x$"Gemeldete Arbeitsstellen"/1e6, start=c(2005,1),
         frequency=12)
plot(ts, main="Gemeldete Arbeitsstellen (in Mio)", xlab="")
