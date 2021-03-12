data(Boston, package="MASS") # Boston Housing Daten
N <- nrow(Boston)            # Umfang Grundgesamtheit
n <- 51                      # Umfang Stichprobe
# Ziehen ohne Zuruecklegen
sample(N, size=n)
# Ziehen mit Zuruecklegen
sample(N, size=n, replace=TRUE)
