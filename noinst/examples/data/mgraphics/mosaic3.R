data <- read.csv("1000 songs.csv", as.is=c(FALSE, TRUE, FALSE, TRUE, TRUE))

data$DECADE <- floor(as.numeric(data$YEAR)/10) * 10
data$BAND <- paste(data$DECADE, "s", sep="")
data$BAND[data$DECADE < 1960] <- "1910s-50s"

tab <- table(data$BAND, data$THEME)
tab2 <- outer(apply(tab,1,sum), apply(tab,2,sum))/sum(tab)
pdf("mosaic3.pdf", bg="transparent")
plot(as.table(tab2), col=rainbow(7), las=1, main="", bg="transparent")
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/","mosaic3.pdf"))