\name{print.attribrisk}
\alias{print.attribrisk}
\title{Prints a summary of attribrisk.}
\description{Print an attribrisk (attributable risk) object.}
\usage{
  \method{print}{attribrisk} (x, ...)
}
\arguments{
  \item{x}{an object of class "attribrisk"}

  \item{...}{further arguments for the final printing such as \code{digits}}
}
\value{a copy of the input, with the invisible flag set to prevent
  printing.
}
\details{
  If there were bootstrap confidence intervals created using multiple
  methods, e.g., 'percentile', 'BCa', etc., then by default only
  the first of them is printed.  To see all confidence intervals
  print the \code{boot.ci} component of the attribrisk object.
  }
\seealso{
  \code{\link{attribrisk}}
}

