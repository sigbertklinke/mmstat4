#' German lotto numbers
#'
#' Lotto numbers 6 out of 49 including bonus and super number starting from 9 Oct 1955.
#'
#' @format A data frame with 4181 rows and 10 variables:
#'
#' 1.  __Datum__  date of drawing
#' 2. __Wochentag__   week day of drawing (in german)
#' 3. __Gewinnzahlen__   first drawn number
#' 4.  __V4__   second drawn number
#' 5.  __V5__   third drawn number
#' 6.  __V6__   fourth drawn number
#' 7.  __V7__   fifth drawn number
#' 8. __V8__   sixth drawn number
#' 9. __Zusatzzahl__   bonus number, always \code{NA}
#' 10.  __Superzahl__   super number, always \code{NA}
#'
"lotto"

#' hhD
#'
#' Development of household shares by household size in Germany.
#'
#' @format A data frame with 54 rows and 7 variables:
#'
#' 1.  __Jahr__ year
#' 2. __Gesamt__ total number of households
#' 3. __Einpersonen__  percentage of one person households
#' 4. __Zweipersonen__  percentage of two person households
#' 5. __Dreipersonen__  percentage of three person households
#' 6. __Vierpersonen__  percentage of four person households
#' 7. __Fuenf und mehr Personen__  percentage of five or more person households
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/)
"hhD"

#' hhB
#'
#' Development of the number of households by household size in Berlin.
#'
#' @format A data frame with 54 rows and 7 variables:
#'
#' 1.  __Jahr__  year
#' 2.  __Privathaushalte__  total number of households (in 1000)
#' 3.  __Einpersonenhaushalte__  number of one person housholds (in 1000)
#' 4.  __Zweipersonenhaushalte__  number of two person housholds (in 1000)
#' 5.  __Dreipersonenhaushalte__  number of three person housholds (in 1000)
#' 6.  __Vierpersonenhaushalte__ number of four person housholds (in 1000)
#' 7.  __5 und mehr__ number of five or more person housholds (in 1000)
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/)
"hhB"

#' data12411
#'
#' Number of males and females by age.
#'
#' @format A data frame with 86 rows and 3 variables:
#'
#' 1.  __V1__  age (in years)
#' 2.  __M__  total number of males in age group
#' 3.  __F__  total number of females in age group
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/)
"data12411"

#' data13211
#'
#' Number of males and females by age.
#'
#' @format A data frame with 86 rows and 3 variables:
#'
#' 1. __Gebiet__  territory
#' 2. __Jahr__  year
#' 3. __Monat__  month
#' 4. __Arbeitslose__ unemployed
#' 4. __Arbeitslosenquote aller zivilen Erwerbspersonen__ unemployment rate based on the total civilian workforce
#' 5. __Arbeitslosenquote d. Abhaengigen ziv. Erwerbspers.__ unemployment rate based on dependant civilian working people
#' 6. __Gemeldete Stellen__ registered vacancies
#' 7. __Kurzarbeiter__ short-time working
#' 8. __Kurzarbeitende Betriebe__ companies that implemented short-time work
#'
#'
#' @source [GENESIS-Online Database](https://www-genesis.destatis.de/genesis/online/)
"data13211"
