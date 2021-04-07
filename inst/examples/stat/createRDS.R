setwd('~/sync/unison/mmstat_en/shiny')
files <- list.files(pattern="*.rds$")
text  <- c()
for (file in files) {
  text <- c(text, gsub('.rds$', '', file))
  x <- readRDS(file)
  for (name in names(x)) {
    text <- c(text, name)
    if (is.factor(x[[name]])) {
      text <- c(text, levels(x[[name]]))
    }
  }
}
# only unique names
text <- unique(text)
text <- text[is.na(as.numeric(text))]
text <- paste0('gettext("', text, '")')
write(text, "rds.R")