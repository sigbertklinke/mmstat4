data(lotto)
samstag <- lotto[lotto["Wochentag"]=="Samstag",]
tab     <- table(unlist(samstag[,3:8]))
tab
sub     <- sprintf("%s bis %s", samstag[nrow(samstag),'Datum'], samstag[1,'Datum'])
barplot(tab, sub=sub, main="Absolute Häufigkeiten für Lotto am Samstag" )
