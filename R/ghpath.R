#' ghdecompose
#'
#' Decomposes a path of a set of files (or dirs) in several parts:
#'
#' * `commonpath` the path part which is common to all files,
#' * `uniquepath` the path part which is unique to all files
#' * `minpath` the minimal path part such that all files addressable in unique manner,
#' * `filename` the basename of the file, and
#' * `source` the input to `shortpath`.
#'
#' @param files character vector: path of files
#' @param dirs logical: directory or files names (default: `FALSE`)
#'
#' @return a data frame with five variables
#' @export
#'
#' @examples
#' ghget("dummy")
#' pdf <- ghdecompose(ghlist(full.names=TRUE))
#' pdf
ghdecompose <- function (files, dirs=FALSE) {
  sfiles <- strsplit(files, "[\\/]")
  path  <- character(0)
  npath <- min(lengths(sfiles))
  for (i in 1:npath) {
    pathi <- unique(sapply(sfiles, '[', i))
    if (length(pathi)>1) break
    path[i] <- pathi
  }
  sfiles     <- lapply(sfiles, function(p) { rev(p[i:length(p)]) })
  upos       <- rep(-1, length(files))
  nonunique  <- (upos<0)
  i          <- 1
  while(any(nonunique)) {
    pfiles <- sapply(sfiles, function(p) { paste0(p[1:min(i,length(p))], collapse="/") })
    dups   <- duplicated(pfiles) | duplicated(pfiles, fromLast=TRUE)
    upos[nonunique & !dups] <- i
    nonunique  <- (upos<0)
    i <- i+1
  }
  uniquepath <- sapply(1:length(files), function(i) { paste0(rev(sfiles[[i]][-(1:upos[i])]), collapse="/")} )
  minpath    <- sapply(1:length(files), function(i) { paste0(rev(sfiles[[i]][1:upos[i]]), collapse="/")} )
  filename   <- rep('', length(files))
  if (!dirs) {
    filename <- basename(minpath)
    minpath  <- dirname(minpath)
  }
  df <- data.frame(commonpath=rep(paste0(path, collapse="/"), length(files)),
             uniquepath=sapply(sfiles, i=i, function(v, i) { if (i<length(v)) paste0(v[i:(length(v)-1)], collapse="/") else '' }),
             minpath=gsub('.', '', minpath, fixed=TRUE),
             filename=filename,
             source=files)
  structure(df, class=c("ghdecompose", class(df)))
}

#' ghpath
#'
#' Returns a path for files based on `ghdecompose`.
#'
#' @param df data frame: returned from `ghdecompose`
#' @param from character: either `minpath` (defaulkt), `commonpath`, `uniquepath`, or `filename`
#'
#' @return a character vector with file pathes
#' @export
#'
#' @examples
#' ghget("dummy")
#' pdf <- ghdecompose(ghlist(full.names=TRUE))
#' ghpath(pdf)
#' ghpath(pdf, 'c') # equals the input to ghdecompose
#' ghpath(pdf, 'u')
#' ghpath(pdf, 'm')
#' ghpath(pdf, 'f')
ghpath <- function(df, from=c("minpath", "commonpath", "uniquepath", "filename")) {
  stopifnot('ghdecompose' %in% class(df))
  from <- match.arg(from)
  p <- apply(df[,which(names(df)==from):4,drop=FALSE], 1, function(r) { paste0(r, collapse="/") })
  p <- gsub("/+", "/", p)
  if (from!="commonpath") {
    abspath <- startsWith(p, '/')
    p[abspath] <- substring(p[abspath], 2)
  }
  p
}
