\name{jump2step}
\alias{jump2step}

\title{Accumulate increments in a step function of time}

\description{
  A utility function for \code{\link{coxic}} that returns the step
  function values given its increments over time.
}

\usage{jump2step(jump, time = NULL, stratum = NULL)}

\arguments{
  \item{jump}{
    A matrix whose columns give increments in the (possibly
    multivariate) step function, corresponding time points and strata.
  }
  \item{time}{
    \code{NULL} (default) or a positive integer indicating column index
    of the time variable in \code{jump}. If \code{stratum} is
    non-\code{NULL}, \code{time} defaults to \code{stratum-1}. Otherwise
    \code{time} defaults to \code{ncol(jump)}.
  }
  \item{stratum}{
    \code{NULL} (default) or a positive integer indicating column index
    of the stratifying variable in \code{jump}. If \code{stratum} is
    \code{NULL}, \code{jump} is assumed un-stratified.
  }
}

\value{A matrix of the same dimension as \code{jump}.}

\seealso{\code{\link{step2jump}}}

\examples{
fit <- coxic(Surv(start, stop, status) ~ cluster(id)
             + trans(from, to) + z, data = dualrc)
haz <- step2jump(fit$fit.coxph[[1]]$basehaz, stratum = 3)
Haz <- jump2step(haz, stratum = 3)
all(Haz == fit$fit.coxph[[1]]$basehaz)
}
