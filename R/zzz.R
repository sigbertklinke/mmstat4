mmstat            <- new.env(parent=emptyenv())
mmstat$data       <- list()
mmstat$lang       <- list()
mmstat$version    <- '22.2'
#
repolist <- paste0(rappdirs::user_data_dir('mmstat4'), '/repositories')
if (file.exists(repolist)) {
  mmstat$repository <- readRDS(repolist)
} else {
  mmstat$repository <- list(
    dummy=list(url="https://github.com/sigbertklinke/mmstat4.dummy/archive/refs/heads/main.zip",
               dir=''),
    hu.data=list(url="https://github.com/sigbertklinke/mmstat4.data/archive/refs/heads/main.zip",
                 dir=''),
    hu.stat=list(url="https://github.com/sigbertklinke/mmstat4.stat/archive/refs/heads/main.zip",
                 dir='')
    )
}
mmstat$repo <- NULL

.onLoad <- function(libname, pkgname) {
  # colors
  options(mmstat.col.population = 'green',
          mmstat.col.sample     = 'orange',
          mmstat.ext.doc        = c('html', 'pdf'),
          mmstat.ext.prg        = c('', 'r', 'rmd', 'ma', 'py')
  )
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage('See the package vignette with `vignette("mmstat4")`')
}
