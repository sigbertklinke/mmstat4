% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ghlist.R
\name{ghlist}
\alias{ghlist}
\alias{ghgrep}
\title{ghgrep, ghlist}
\usage{
ghlist(
  pattern = ".",
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  full.names = FALSE
)

ghgrep(
  pattern = ".",
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE,
  full.names = FALSE
)
}
\arguments{
\item{pattern}{character string containing a \link[base]{regular expression}
    (or character string for \code{fixed = TRUE}) to be matched
    in the given character vector.  Coerced by
    \code{\link[base]{as.character}} to a character string if possible.  If a
    character vector of length 2 or more is supplied, the first element
    is used with a warning.  Missing values are allowed except for
    \code{regexpr}, \code{gregexpr} and \code{regexec}.}

\item{ignore.case}{if \code{FALSE}, the pattern matching is \emph{case
      sensitive} and if \code{TRUE}, case is ignored during matching.}

\item{perl}{logical.  Should Perl-compatible regexps be used?}

\item{fixed}{logical.  If \code{TRUE}, \code{pattern} is a string to be
    matched as is.  Overrides all conflicting arguments.}

\item{useBytes}{logical.  If \code{TRUE} the matching is done
    byte-by-byte rather than character-by-character.  See
    \sQuote{Details}.}

\item{full.names}{logical: should full names returned instead of short names (default: \code{FALSE})}
}
\value{
character vector of short names
}
\description{
Both functions return unique (short) names for accessing each file in the repository according to a regular expression.
For details about regular expressions, see \link[base:regex]{base::regex}.
}
\examples{
if (interactive()) ghgrep()
}
