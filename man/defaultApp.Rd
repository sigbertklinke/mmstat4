% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gh.R
\name{defaultApp}
\alias{defaultApp}
\title{defaultApp}
\usage{
defaultApp(file, wait = FALSE, ...)
}
\arguments{
\item{file}{character: file name}

\item{wait}{logical: indicates whether the R interpreter should wait for the command to finish, or run it asynchronously (default: \code{FALSE})}

\item{...}{further arguments passed to \code{system2}}
}
\value{
Result of \code{try(system2, ...)}, invisibly
}
\description{
Tries to open the given \code{file} with the default application of the operating system using \code{\link[base:system2]{base::system2()}}.
Only Windows (\code{windows}), macOS (\code{darwin}), Linux (\code{linux}) and FreeBSD (\code{freebsd}) is supported.
}
\examples{
if (interactive()) {
  ghget()
  defaultApp(ghlist("dataanalysis.pdf", full.names = TRUE))
}
}
\seealso{
\href{https://CRAN.R-project.org/package=berryFunctions}{\code{berryFunctions::openFile()}}
}
