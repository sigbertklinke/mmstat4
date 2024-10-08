% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gh.R
\name{gh}
\alias{gh}
\alias{ghopen}
\alias{ghload}
\alias{ghsource}
\alias{ghdata}
\alias{ghapp}
\title{gh functions}
\usage{
gh(x, what = c("open", "load", "source", "app"), ..., .call = NULL)

ghopen(x, ...)

ghload(x, ...)

ghsource(x, ...)

ghapp(x, ...)
}
\arguments{
\item{x}{character(1): name of the file, app or data set}

\item{what}{character or function: a name of a predefined function or another function. The function must have a formal parameter \code{file}.}

\item{...}{further parameters used in \code{\link[utils:browseURL]{utils::browseURL()}}, \code{\link[=openFile]{openFile()}}, \code{\link[rio:import]{rio::import()}}, or \code{\link[base:source]{base::source()}}.}

\item{.call}{the original function call (default: \code{NULL})}
}
\value{
invisibly the result of \link[utils:browseURL]{utils::browseURL}, \code{\link[=openFile]{openFile()}}, \code{\link[rio:import]{rio::import()}}, or \code{\link[base:source]{base::source()}}.
}
\description{
The function \code{gh} carries out the following operation on a file named \code{x}.
It searches for a match for \code{x} within the active repository, utilizing fuzzy string
matching. If no unique match is identified, an error is thrown along with suggestions for
potential "best" matches.
Otherwise, the following operation are performed:
\itemize{
\item \code{gh(x, 'open')} or \code{ghopen(x)}: Opens a file in the local browser if the file extension is \code{html} or \code{pdf}, otherwise in the RStudio editor.
\item \code{gh(x, 'load')} or \code{ghload(x)}: Loads the contents of a file with \code{import} and \code{trust=TRUE}.
\item \code{gh(x, 'source')} or \code{ghsource(x)}: Executes the contents of a file with \code{source}.
\item \code{gh(x, 'app')} or \code{ghapp(x)}: Tries to open the file with the default application of the OS, see \code{\link[=defaultApp]{defaultApp()}}.
\item \code{ghdata(x, pkg)}: Helper function  to load data sets from R packages into Python, simulates \code{pkg::x}.
}
}
\examples{
if (interactive()) {
  x <- ghopen("bank2.SAV")
  x <- ghload("bank2.SAV")
  str(x)
  x <- ghsource("univariate/example_ecdf.R")
}
}
