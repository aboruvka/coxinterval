\name{const2lin}
\alias{const2lin}

\title{Integral values from a piecewise constant function of time}

\description{
  A utility function for \code{\link{coxic}} that returns the integral
  of a piecewise constant function of time, evaluated at given values.
}

\usage{const2lin(const, time = NULL, stratum = NULL)}

\arguments{
  \item{const}{
    A matrix whose columns give the (possibly multivariate) piecewise
    constant function values, corresponding time points and strata.
  }
  \item{time}{
    \code{NULL} (default) or a positive integer indicating column index
    of the time variable in \code{const}.  If \code{stratum} is
    non-\code{NULL}, \code{time} defaults to \code{stratum-1}.  Otherwise
    \code{time} defaults to \code{ncol(const)}.
  }
  \item{stratum}{
    \code{NULL} (default) or a positive integer indicating column index
    of the stratifying variable in \code{const}.  If \code{stratum} is
    \code{NULL}, \code{const} is assumed unstratified.
  }
}

\value{A matrix of the same dimension as \code{const}.}

\seealso{\code{lin2const}}

\examples{
fit <- coxic(Surv(start, stop, status) ~ cluster(id)
             + trans(from, to) + z, data = dualrc)
haz <- lin2const(fit$basehaz, stratum = 3)
Haz <- const2lin(haz, stratum = 3)
all(Haz == fit$basehaz)
}

\keyword{survival}
