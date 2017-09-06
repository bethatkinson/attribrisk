\name{attribrisk.control}
\alias{attribrisk.control}
\title{ Sets optional parameters for attribrisk. }
\description{  Sets optional bootstrap parameters for attribrisk. }
\usage{ attribrisk.control(nboot=500, bootci=list(type="perc") ) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nboot}{ Number of bootstrap samples }
  \item{bootci}{ Parameters passed to the \code{boot.ci} function. }
}

\value{ A \code{list} of values that will be paaaed into \code{boot} and \code{boot.ci}}

\seealso{
\code{\link[boot]{boot}},
\code{\link[boot]{boot.ci}}
}
