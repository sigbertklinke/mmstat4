## code to prepare `DATASET` dataset goes here
selectvars <- function(df, vars, asis=TRUE) {
  if (!is.null(vars)) df <- df[vars]
  if (!asis) {
    df <- df[complete.cases(df),]
  }
  print(sapply(df, class))
  df
}
#
library("rio")
patterns <- c("\\.csv$", "\\.SAV$", "\\.sav$", "\\.dta$", "\\.ods$", "\\.xls$",  "\\.xlsx$")
exds     <- c("iris", "allbus2010", "allbus2014", "europa", "cardata", "heliumfootball", "manners",
              "mhne", "mikrozensus2002", "six_of_49", "titanic_statisticalmodeling", "wages", "wartezeiten")

for (pat in patterns) {
  files <- list.files(pattern=pat, recursive=TRUE, full.name=TRUE)
  for (file in files) {
    print (file)
    ds  <- tolower(strsplit(basename(file), ".", fixed=TRUE)[[1]][1])
    ds  <- gsub("( |\\-)", "_", ds)
    vars <- NULL
    asis <- (ds!="pechstein")
    fmt  <- if (ds %in% c("pechstein", "cps78_85", "wages")) ", \"csv2\"" else ""
    if (ds=="allbus2012") {
      vars <- c("v220", "v593", "v595")
    }
    if (ds=="allbus2018") {
      vars <- c("eastwest","ep03", "ep06", "di05", "di06", "hhinc", "land",
                "pt01", "pt02", "pt03", "pt04", "pt08", "pt09", "pt10",
                "pt11", "pt12", "pt14", "pt15", "pt19", "pt20")
    }
    if (!(ds %in% exds) && !grepl("^[0-9]", ds)) {
      cmd <- sprintf("%s <- selectvars(import(file%s), vars, %s); usethis::use_data(%s, overwrite = TRUE)",
                     ds, fmt, as.character(asis), ds)
      eval(parse(text=cmd))
    }
  }
}

