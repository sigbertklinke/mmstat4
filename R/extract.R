#' @rdname extract
#' @title extract
#' @description \code{extract} extract programs from a set of LaTeX files and returns them in a list.
#' ```{r latex, eval=FALSE}
#' \begin{comment}{helloworld.R}
#' "Hello world!"
#' \end{comment}
#' ```
#' \code{print} and \code{summary} give an overview about the extracted programs.
#' \code{do.extract} will do the following:
#'
#'  * \code{what="parse"} parses the codes extracted from \code{*.R} comments.
#'  * \code{what="diff"} diffs the codes extracted from \code{*.R} comments.
#'  * \code{what="eval"} parses and evaluates the codes in the directory of the LaTeX file.
#'  * \code{what="write"} writes the codes extracted from \code{*.R} comments to a file.
#'  * \code{what="local"} writes the codes extracted from \code{*.R} comments to a file in the directory of the LaTeX file.
#'
#'
#' @param files character: name of LaTeX files
#' @param x extract object
#' @param object extract object
#' @param what character: what to do with the program codes (default: \code{"parse"})
#' @param quiet logical: should be \code{extract} be quiet? (default: \code{TRUE})
#' @param verbose logical: should be \code{do.extract} give full output? (default: \code{FALSE})
#' @param ... further parameters (unused)
#'
#' @return extract object
#' @export
#'
#' @examples
#' x <- 1
extract <- function(files, ...) { UseMethod("extract")}

#' @rdname extract
#' @importFrom stringr str_match_all regex
#' @export
extract.default <- function(files, ..., quiet=TRUE) {
  cwd     <- getwd()
  if (!quiet) cat("\n")
  #browser()
  tryCatch({
    extfile <- list()
    args    <- list(...)
    k       <- 1
    for (i in seq(files)) {
      file   <- files[[i]]
      dir    <- dirname(file)
      base   <- basename(file)
      #browser()
      setwd(dir)
      path   <- normalizePath(getwd())
      source <- paste0(path, '/', base)
      if (file.exists(source)) {
        fcont <- paste0(readLines(source), collapse="\n")
        match <- str_match_all(fcont, regex('\\\\begin\\{comment\\}(\\[(.*?)\\])*\\{(.*?)\\}(.*?)\\\\end\\{comment\\}', dotall=TRUE))[[1]]
        if (length(match)) {
          extfile[[k]] <- list(source=source, opt=match[,3], file=match[,4], content=trimws(match[,5], "left"))
          k <- k+1
        }
      } else {
        cat ("  File not found:", source, "\n")
      }
      setwd(cwd)
    }
  },
  error = function(e) {
    setwd(cwd)
    stop(e)
  })
  if (length(extfile)==0) warning("No tex or programs files found")
  invisible(structure(extfile, class="extract"))
}

#' @rdname extract
#' @export
print.extract <- function(x, ...) {
  if (length(x)) {
    cat("\n")
    for (i in 1:length(x)) {
      cat(x[[i]]$source, "\n")
      cat(" ", paste0(x[[i]]$file, collapse=", "), "\n")
    }
  }
}

#' @rdname extract
#' @export
summary.extract <- function(object, ...) {
  if (length(object)) {
    df <- data.frame(prgs=sapply(object, function(e) { length(e$file) }),
                     source=sapply(object, '[[', "source"))
    print(df)
  }
}

# Sys.setFileTime("test.txt", "1980-12-04")
#' @rdname extract
#' @importFrom digest digest
#' @importFrom crayon red
#' @export
do.extract <- function(what = c("parse", "eval", "diff", "write", "local"), x, verbose=FALSE) {
  stopifnot("extract" %in% class(x))
  if (length(x)) {
    what <- match.arg(what)
    df   <- data.frame(source  = unlist(lapply(x, function(e) { rep(e$source, length(e$file))})),
                       file    = unlist(lapply(x, '[[', 'file')),
                       opt     = unlist(lapply(x, '[[', 'opt')),
                       content = unlist(lapply(x, '[[', 'content'))
    )
    df$rfile <- endsWith(df$file, ".R")
    cwd <- getwd()
    msg <- NULL
    cat("\n")
    lastsource <- ""
    for (i in 1:nrow(df)) {
      tryCatch({
        #
        if (what=="eval") {
          if (df$rfile[i]) {
            tryCatch({
              setwd(dirname(df$source[i]))
              eval(str2expression(df$content[i]), envir=new.env())
            },
            error= function(e) {
              if (lastsource!=df$source[i]) {
                cat(df$source[i], "\n")
                lastsource <- df$source[i]
              }
              msg <- paste0("  ", df$file[i], ": ", e$message, "\n")
              cat (red(msg))
            },
            warning = function(w) {
              if (lastsource!=df$source[i]) {
                cat(df$source[i], "\n")
                lastsource <- df$source[i]
              }
              cat (" ", df$file[i], ": ")
              cat (w$message, "\n")
            },
            finally=setwd(cwd))
          } else {
            if (verbose) warning("file ignored")
          }
        }
        #
        if (what=="parse") {
          if (df$rfile[i]) {
            str2expression(df$content[i])
          } else {
            if (verbose) warning("file ignored")
          }
        }
        #
        if (what=="local") {
          tryCatch({
            srcinfo <- file.info(df$source[i])
            setwd(dirname(df$source[i]))
            writeLines(df$content[i], basename(df$file[i]))
            Sys.setFileTime(basename(df$file[i]), srcinfo$mtime)
          },
          error= function(e) {
            if (lastsource!=df$source[i]) {
              cat(df$source[i], "\n")
              lastsource <- df$source[i]
            }
            msg <- paste0("  ", df$file[i], ": ", e$message, "\n")
            cat (red(msg))
          },
          warning = function(w) {
            if (lastsource!=df$source[i]) {
              cat(df$source[i], "\n")
              lastsource <- df$source[i]
            }
            cat (" ", df$file[i], ": ")
            cat (w$message, "\n")
          },
          finally=setwd(cwd))
        }
        #
        if (what=="write") {
          tryCatch({
            srcinfo <- file.info(df$source[i])
            setwd(dirname(df$source[i]))
            src <- normalizePath(df$file[i])
            if (!dir.exists(dirname(src))) dir.create(dirname(src), recursive = TRUE)
            writeLines(df$content[i], src)
            Sys.setFileTime(src, srcinfo$mtime)
          },
          error= function(e) {
            if (lastsource!=df$source[i]) {
              cat(df$source[i], "\n")
              lastsource <- df$source[i]
            }
            msg <- paste0("  ", df$file[i], ": ", e$message, "\n")
            cat (red(msg))
          },
          warning = function(w) {
            if (lastsource!=df$source[i]) {
              cat(df$source[i], "\n")
              lastsource <- df$source[i]
            }
            cat (" ", df$file[i], ": ")
            cat (w$message, "\n")
          },
          finally=setwd(cwd))
        }
        #
        if (what=="diff") {
          srcinfo <- file.info(df$source[i])
          if (!dir.exists(dirname(df$source[i]))) stop("dir not found")
          setwd(dirname(df$source[i]))
          src <- normalizePath(df$file[i], mustWork = TRUE)
          if (!file.exists(src)) stop("file not found")
          if (digest(df$content[i], serialize=FALSE) != digest(file=src)) {
            finfo <- file.info(src)
            if ((srcinfo$mtime>finfo$mtime) && verbose) warning("file is older than tex file")
            if (srcinfo$mtime<finfo$mtime) stop("file is younger than tex file")
          }
        }
      },
      error= function(e) {
        if (lastsource!=df$source[i]) {
          cat(df$source[i], "\n")
          lastsource <- df$source[i]
        }
        msg <- paste0("  ", df$file[i], ": ", e$message, "\n")
        cat (red(msg))
      },
      warning = function(w) {
        if (lastsource!=df$source[i]) {
          cat(df$source[i], "\n")
          lastsource <- df$source[i]
        }
        cat (" ", df$file[i], ": ")
        cat (w$message, "\n")
      },
      finally=setwd(cwd))
    }
  }
}
