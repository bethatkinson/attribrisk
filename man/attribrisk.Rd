\name{attribrisk}
\alias{attribrisk}
\title{Calculate attributable risk estimates for one or more exposure
  characteristics.}
\description{Calculate attributable risk estimates for one or more exposure
  characteristics.  The attributable risk, or etiologic fraction, is an
  estimate of the reduction in an outcome were a risk factor to change.}
\usage{
  attribrisk(formula, data, weights, subset, na.action,
    varmethod = c("jackknife", "bootstrap", "none"),
    conf=.95, baseline, k=20, control,
    model = FALSE, x = FALSE, y = FALSE, ...)
}
\arguments{
  \item{formula}{an object of class 'formula'.  A symbolic
  description of the model to be fitted.}

  \item{data}{a data frame used for the formula.}

  \item{weights}{optional weights for the fitting
  criterion.}

  \item{subset}{an optional vector specifying a subset of
  observations to be used.}
  \item{na.action}{
    a missing-data filter function.  This is applied to the model.frame
    after any 
    subset argument has been used.  Default is \code{options()\$na.action}. 
  }

  \item{varmethod}{A string that specifies the resampling
  technique used to estimate confidence intervals and
  standard errors.  \itemize{ \item{bootstrap:}{ indicates that
  the CI and standard error should be estimated using a
  bootstrap.} \item{jackknife:}{ indicates that the CI and
  standard error should be estimated using a grouped jackknife.}
  \item{none:}{ do not estimate standard error or CI.} }}

  \item{k}{the number of groups to use for the jackknife. 
    The parameter is ignored for bootstrap variance.  Setting this
    to 0 or to a value >= the sample size will leads to leaving out each
    observation one at a time, i.e., the ordinary jackknife.
    Optionally, \code{k} can be a vector with one element per observation
    that directly specifies the grouping of the observation, the
    jackknife estimate will leave out one group at a time.  If the model
    has strata then they will not be broken, either all or none of the
    observations in a strata are left out of each jackknife subsample.}
    
  \item{conf}{The confidence level for confidence intervals}

  \item{control}{a list of optional parameters, see \code{attribrisk.control}.}

  \item{baseline}{Must be either NULL or a data frame containing values
    for the exposure variable(s) of the formula, which specifies the
    desired baseline value for each individual.}

  \item{model}{a logical value indicating whether model
  frame should be included as a component of the returned
  value.}

  \item{x,y}{logical values indicating whether the model
    matrix and/or response used in the fitting process should be returned.}
  \item{\dots}{other arguments such as \code{nboot},
    normally passed to the \code{attribrisk.control} routine.}

}
\value{
  an object of class "attribrisk" with the following
  components:
  \describe{
    \item{attribrisk}{attributable risk estimate}
    \item{var}{variance of the attributable risk}
    \item{fit}{results from the underlying coxph or glm fit}
    \item{boot}{results of the \code{boot} function, optional}
    \item{boot.ci}{results of the \code{boot.ci} function, optional}
    \item{call}{ A copy of the call to the function} }
}
\section{Details}{
  None.
}
\examples{

data(benichou)

# Use the Benichou (1991) data to estimate attributable risk of oesophageal
# cancer due to alcohol greater than or equal to 80g/day 
attribrisk(cases ~ expos(alcohol80), data=benichou)
}
\seealso{
  \code{\link{attribrisk.fit}}, \code{\link{attribrisk.control}}, and
  \code{\link{benichou}}
}

