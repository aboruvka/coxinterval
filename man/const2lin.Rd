\name{const2lin}
\alias{const2lin}

\title{Accumulate values from a piecewise constant function of time}

\description{
  A utility function for \code{\link{coxdual}} that integrates values
  from a piecewise constant function of time.
}

\usage{const2lin(const, time = "time", strata = "strata")}

\arguments{
  \item{const}{
    a matrix whose columns give the (possibly multivariate) piecewise
    constant function values, corresponding time points and strata.
  }
  \item{time}{
    an integer or character value indicating the column index or name of
    the time variable in the matrix \code{const}.  Defaults to \code{"time"}.
  }
  \item{strata}{
    integer or character value for the column index or name of the
    stratifying variable in the matrix \code{const}.  Defaults to
    \code{"strata"}.  If any character value provided is not a column
    name, \code{const} is presumed unstratified.
  }
}

\value{A matrix of the same dimension as \code{const}.}

\seealso{\code{lin2const}}

\examples{
fit <- coxdual(Surv(start, stop, status) ~ cluster(id)
               + strata(from, to) + z, data = dualrc, init.coxph = TRUE)
haz <- lin2const(fit$basehaz)
Haz <- const2lin(haz)
all(Haz == fit$basehaz)
}

\keyword{survival}
