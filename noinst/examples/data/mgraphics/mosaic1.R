data <- read.csv("1000 songs.csv", as.is=c(FALSE, TRUE, FALSE, TRUE, TRUE))

data$DECADE <- floor(as.numeric(data$YEAR)/10) * 10
data$BAND <- paste(data$DECADE, "s", sep="")
data$BAND[data$DECADE < 1960] <- "1910s-50s"

pdf("mosaic1.pdf", bg="transparent")
plot(table(data$BAND, data$THEME), col=rainbow(7), las=1, main="", bg="transparent")
dev.off()
if (interactive()) browseURL(paste0(getwd(),"/","mosaic1.pdf"))