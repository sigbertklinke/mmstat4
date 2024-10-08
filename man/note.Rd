% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/note.R
\name{note}
\alias{note}
\alias{display}
\title{Create and display a note}
\usage{
note(msg, col = crayon::green)

display()
}
\arguments{
\item{msg}{character: message}

\item{col}{function: a color function (default: \code{crayon::green})}
}
\value{
\code{note} returns invisibly the number of notes
}
\description{
\code{note} internally stores a colored message, while \code{display} utilizes \code{\link[base:cat]{base::cat()}} to present them
and reset the internal message stack.
}
\examples{
notetest <- function(msg) {
  on.exit({ display() })
  note(msg)
  # do some complex computation
  x <- 1+1
}
notetest("Hello world!")
}
