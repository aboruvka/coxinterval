\name{const2lin}
\alias{const2lin}

\title{Integral values from a piecewise constant function of time}

\description{
  Returns values from the piecewise linear cumulant
  corresponding to given values from a piecewise constant function of
  time.
}

\usage{const2lin(const, time = NULL, stratum = NULL)}

\arguments{
  \item{const}{
    A matrix whose columns give the (possibly multivariate) piecewise
    constant function values, time points and strata.
  }
  \item{time}{
    \code{NULL} (default) or a positive integer indicating column index
    of the time variable in \code{const}. If \code{stratum} is
    non-\code{NULL}, \code{time} defaults to \code{stratum-1}. Otherwise
    \code{time} defaults to \code{ncol(const)}.
  }
  \item{stratum}{
    \code{NULL} (default) or a positive integer indicating column index
    of the stratifying variable in \code{const}. If \code{stratum} is
    \code{NULL}, \code{const} is assumed unstratified.
  }
}

\value{A matrix of the same dimension as \code{const}.}

\details{
  This is a utility function for \code{\link{coxic}}.
}

\seealso{\code{lin2const}}

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
