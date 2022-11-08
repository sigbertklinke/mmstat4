mmstat            <- new.env(parent=emptyenv())
mmstat$data       <- list()
mmstat$lang       <- list()
mmstat$version    <- '22.2'
mmstat$repo       <- "mmstat4"
#
repolist <- paste0(rappdirs::user_data_dir('mmstat4'), '/repositories')
if (file.exists(repolist)) {
  mmstat$repository <- readRDS(repolist)
} else {
  mmstat$repository <- list(mmstat4=list(url="https://github.com/sigbertklinke/mmstat4.data/archive/refs/heads/main.zip",
                                         dir=''),
                            dummy=list(url="https://github.com/sigbertklinke/mmstat4.dummy/archive/refs/heads/main.zip",
                                       dir=''))
}



.onLoad <- function(libname, pkgname) {
  # colors
  options(mmstat.col.population = 'green',
          mmstat.col.sample     = 'orange',
          mmstat.ext.doc        = c('html', 'pdf'),
          mmstat.ext.prg        = c('', 'r', 'rmd', 'ma', 'py'),
          mmstat.repo           = "mmstat4"
  )
}
