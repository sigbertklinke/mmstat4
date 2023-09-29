#' @title ghquery
#' @description Queries the unique (short) names for each file in the repository.
#' Several query methods are available, see Details.
#' @details The following query methods are available:
#' * `fpdist` uses a partial backward matching distance based on [utils::adist()]
#' * `overlap` uses the \href{https://en.wikipedia.org/wiki/Overlap_coefficient}{overlap distance} for `query` and file names
## * `tfidf` uses a \href{https://en.wikipedia.org/wiki/Tf%E2%80%93idf}{TF-IDF} approach interpreting the filenames and the query as documents
#' @param query character: query string
#' @param n integer: maximal number of matches to return
#' @param full.names logical: should full names used instead of short names (default: `FALSE`)
#' @param method character: method to be used (default: `fpdist`)
#' @inheritParams utils::adist
#'
#' @return character vector of short names fitting best to the query
# @importFrom text2vec itoken vocab_vectorizer create_vocabulary create_vocabulary TfIdf fit_transform create_dtm sim2
#' @export
#'
#' @examples
#' if (interactive()) ghquery("bank")
ghquery <- function(query, n=6, full.names=FALSE, method=c("fpdist", "overlap", "tfidf"),
                    costs = NULL, counts = FALSE, useBytes = FALSE) {
  overlap <- function(files, x, n) {
    x   <- unique(tolower(strsplit(x, "")[[1]]))
    f   <- strsplit(files, "")
    dxl <- sapply(f, x=x, function(v, x) {
      v <- unique(tolower(v))
      1-length(intersect(v,x))/min(length(v),length(x))
    })
    order(dxl, nchar(files), decreasing=FALSE)
  }
  #
  match.query <- function(files, query, costs = NULL, counts = FALSE, useBytes = FALSE) {
    fp    <- normpathes(tolower(files))
    fpl   <- unique(unlist(lapply(fp, function(v) { v[length(v)] })))
    term  <- unique(unlist(fp))
    last  <- !(term %in% fpl) # actually not last ;)
    query <- rev(normpathes(query)[[1]])
    lx    <- nchar(query)
    args  <- vector("list", length(query))
    for (i in 1:length(query)) {
      voc <- if (i==1) term else term[last]
      d <- 0.05
      while(length(args[[i]])==0) {
        args[[i]] <- agrep(query[i], voc, fixed=TRUE, value=TRUE, max.distance=d,
                           costs = costs, useBytes = useBytes)
        d <- d+0.05
      }
    }
    args$stringsAsFactors <- FALSE
    args <- as.list(do.call(expand.grid, rev(args)))
    args$sep <- "/"
    structure(do.call(paste, args), term=term)
  }
  #
  fpdist <- function(files, query, n, costs = NULL, counts = FALSE, useBytes = FALSE) {
    queries <- match.query(lof, query, costs = costs, counts = counts, useBytes = useBytes)
    xs   <- normpathes(tolower(files))
    lx   <- lengths(xs)
    ys   <- normpathes(tolower(queries))
    ly   <- lengths(ys)
    d    <- matrix(NA, nrow=length(xs), ncol=length(ys))
    lmax <- max(lengths(xs), lengths(ys))
    for (i in 1:length(xs)) {
      for (j in 1:length(ys)) {
        px  <- lx[i]
        py  <- ly[j]
        dij <- rep(0, lmax)
        for(k in 1:lmax) {
          if ((px<1) || (py<1)) break
          dij[k] <- adist(xs[[i]][px], ys[[j]][py], costs = costs, counts = counts, useBytes=useBytes)
          px <- px-1
          py <- py-1
        }
        d[i,j] <- sum(dij)
      }
    }
    ds    <- sort(unique(as.numeric(d)))
    index <- c()
    for (i in 1:length(ds)) {
      ind   <- which(d==ds[i], arr.ind = TRUE)
      index <- unique(c(index, ind[,'row']))
      if (length(index)>=n) break
    }
    index
  }
  #
  #tfidf <- function(files, query, n, costs = NULL, counts = FALSE, useBytes = FALSE) {
  #  q     <- match.query(files, query, costs = costs, counts = counts, useBytes = useBytes)
  #  # text2vec
  #  train      <- itoken(files, preprocessor=tolower, tokenizer = normpathes, progressbar = FALSE)
  #  vectorizer <- vocab_vectorizer(create_vocabulary(train))
  #  dtm_train  <- create_dtm(train, vectorizer)
  #  tfidf      <- TfIdf$new()
  #  doctfidf   <- fit_transform(dtm_train, tfidf)
  #  #
  #  test     <- itoken(q, tokenizer = normpathes, progressbar = FALSE)
  #  dtm_test <- create_dtm(test, vectorizer)
  #  qrytfidf <- transform(dtm_test, tfidf)
  #  dist <- sim2(dtm_train, dtm_test, method = "cosine", norm = "l2")
  #  colN <- diff(dist@p) #get the number of non-zero elements in each column
  #  indx <- cbind(dist@i+1,rep(seq_along(colN),colN)) #create the indices of all non-zero elements
  #  ival <- dist[indx]
  #  idist <- sort(unique(ival), decreasing = TRUE)
  #  index <- c()
  #  for (i in 1:length(idist)) {
  #    pos   <- which(ival==idist[i])
  #    index <- unique(c(index, indx[pos,1]))
  #    if (length(index)>=n) break
  #  }
  #  index
  #}
  #
  stopifnot(length(query)==1)
  method  <- match.arg(method)
  lof     <- ghlist(full.names = full.names)
  if (method=="overlap") index <- overlap(lof, query, n)
  if (method=="fpdist")  index <- fpdist(lof, query, n, costs = costs, counts = counts, useBytes = useBytes)
#  if (method=="tfidf")   index <- tfidf(lof, query, n, costs = costs, counts = counts, useBytes = useBytes)
  n   <- min(n, length(index))
  lof[index[1:n]]
}
