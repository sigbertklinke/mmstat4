% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/openFile.R
\name{openFile}
\alias{openFile}
\title{openFile}
\usage{
openFile(file, ...)
}
\arguments{
\item{file}{character: name of the file}

\item{...}{further parameters give to \code{\link[rstudioapi:navigateToFile]{rstudioapi::navigateToFile()}} or \code{\link[utils:edit]{utils::edit()}}}
}
\value{
invisibly the result from \code{try(rstudioapi::navigateToFile(file))} or \code{try(utils::edit(file))}.
}
\description{
The function attempts to open a file either in RStudio or in a text editor, depending on the environment.
If the session is interactive, it tries to open the file in RStudio using \code{\link[rstudioapi:navigateToFile]{rstudioapi::navigateToFile()}}.
If RStudio is not available or the attempt fails, it opens the file in a text editor using \code{\link[utils:edit]{utils::edit()}}.
If the session is not interactive, it simply returns the contents of the file.
}
\examples{
openFile(system.file("rstudio", "addins.dcf", package = "mmstat4"))
}
