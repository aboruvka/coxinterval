\name{maximalint}
\alias{maximalint}

\title{Find maximal intersections from an interval-type survival object}

\description{
  A utility function for \code{\link{coxaalenic}} that returns the
  "maximal intersections" from a set of censoring intervals in a given
  \code{type = "interval"} or \code{type = "interval2"}
  \code{\link[survival]{Surv}} object.
}

\usage{maximalint(x, size = FALSE)}

\arguments{
  \item{x}{
    a survival object returned by the \code{\link[survival]{Surv}}
    function with argument \code{type = "interval"} or
    \code{type = "interval2"}
  }
  \item{size}{
    logical value indicating whether or not only the number of maximal
    intersections should be returned by the function
  }
}

\value{
  If \code{size = FALSE}, a named list with items:
  \item{int}{a two-column matrix whose rows give the maximal
  intersections (Wong and Yu, 1999) of the censoring intervals
  represented in \code{x}}
  \item{ind}{a \code{nrow(x)} by \code{nrow(int)} matrix that indicates
  overlap between each maximal intersection and censoring interval}
  Otherwise, only the number of maximal intersections (\code{nrow(int)})
  are returned.
}

\details{
  Each censoring interval is assumed to exclude its left
  endpoint. Following Maathuis (2005, Section 2.1) ties between the
  unique (open) left and (closed) right endpoints among the censoring
  intervals are broken by subtracting a small value from the right
  endpoint value before constructing the maximal intersections.
}

\references{
  Maathuis, MH (2005)
  \href{http://dx.doi.org/10.1198/106186005X48470}{
    Reduction algorithm for the NPMLE for the distribution function
    of bivariate interval-censored data}.
  \emph{Journal of Computational and Graphical Statistics}
  \bold{14}, 352--362.

  Wong, GYC. and Yu, Q. (1999)
  \href{http://dx.doi.org/10.1006/jmva.1998.1807}{
    Generalized MLE of a joint distribution function with multivariate
    interval-censored data}.
  \emph{Journal of Multivariate Analysis} \bold{69}, 155--166.
}

\examples{
s <- with(cosmesis[1:10, ], Surv(left, right, type = "interval2"))
maximalint(s)
maximalint(s, size = TRUE)
}
