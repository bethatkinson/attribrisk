\name{attribrisk.fit}
\alias{attribrisk.fit}
\title{This function is used internally by attribrisk functions.}
\usage{
  attribrisk.fit(x, y, w, offset, match, xbase,
    fit = FALSE)
}
\arguments{
  \item{x}{is a design matrix of dimension "n * p".}

  \item{y}{is a vector of observations of length "n".}

  \item{w}{optional weights for the fitting criterion.}

  \item{offset}{this can be used to specify an component to
  be included in the linear predictor during fitting.  This
  should be NULL or a numeric vector of length equal to the
  number of observations.}

\item{match}{a vector of length n that indicates which observations
  are a part of each match; NULL for an unmatched analysis.
  The values should be integers from 1 to the number of matched sets.}
 
\item{xbase}{the x matrix, minus the values used as a baseline for exposure}

\item{fit}{if TRUE then return the underlying glm or coxph fit object as
  well as the attributable risk estimate}
 }
\description{
  This function is used internally by attribrisk functions.
}
\seealso{\code{attribrisk}}


