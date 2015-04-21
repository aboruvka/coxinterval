\name{maximalint}
\alias{maximalint}

\title{Find maximal intersections}

\description{
  Returns maximal intersections for a given interval-type Surv object.
}

\usage{maximalint(x, eps = 1e-7)}

\arguments{
  \item{x}{
    an \code{type = "interval"} object returned by the
    \code{\link[survival]{Surv}} function.
  }
  \item{eps}{
    a small value used to break ties in maximal intersection endpoints.
  }
}

\value{
  \item{int}{
    a matrix giving left and right endpoints of the maximal intersections.
  }
  \item{ind}{
    matrix indicating overlap between each maximal intersection and
    censoring interval.
  }
}

\examples{
head(cosmesis, 10)
with(cosmesis[1:10, ], maximalint(Surv(left, right, type = "interval2")))
}

\keyword{survival}
