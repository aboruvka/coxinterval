\name{lin2const}
\alias{lin2const}

\title{Derivative values from a piecewise linear function of time}

\description{
  A utility function for \code{\link{coxic}} that returns the derivative
  of a piecewise linear function of time, evaluated at given values.
}

\usage{lin2const(lin, time = NULL, stratum = NULL)}

\arguments{
  \item{lin}{
    A matrix whose columns give the (possibly multivariate) piecewise
    linear function values, time points and strata.
  }
  \item{time}{
    \code{NULL} (default) or a positive integer indicating column index
    of the time variable in \code{lin}. If \code{stratum} is
    non-\code{NULL}, \code{time} defaults to \code{stratum-1}. Otherwise
    \code{time} defaults to \code{ncol(lin)}.
  }
  \item{stratum}{
    \code{NULL} (default) or a positive integer indicating column index
    of the stratifying variable in \code{lin}. If \code{stratum} is
    \code{NULL}, \code{lin} is assumed unstratified.
  }
}

\value{A matrix of the same dimension as \code{lin}.}

\details{This is a utility function for \code{\link{coxic}}.}

\seealso{\code{const2lin}}

\examples{
data(dualrc)
fit <- coxic(Surv(start, stop, status) ~ cluster(id) + trans(from,
to) + z, data = dualrc)
fit$bhaz
haz <- lin2const(fit$bhaz, stratum = 3)
Haz <- const2lin(haz, stratum = 3)
all(Haz == fit$bhaz)
}

\keyword{survival}
