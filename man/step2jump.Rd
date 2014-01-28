\name{step2jump}
\alias{step2jump}

\title{Increments from a step function of time}

\description{
  A utility function that returns the increments in a step function of time.
}

\usage{step2jump(step, time = NULL, stratum = NULL)}

\arguments{
  \item{step}{
    A matrix whose columns give the (possibly multivariate) step
    function values, corresponding time points and strata.
  }
  \item{time}{
    \code{NULL} (default) or a positive integer indicating column index
    of the time variable in \code{step}. If \code{stratum} is
    non-\code{NULL}, \code{time} defaults to \code{stratum-1}. Otherwise
    \code{time} defaults to \code{ncol(step)}.
  }
  \item{stratum}{
    \code{NULL} (default) or a positive integer indicating column index
    of the stratifying variable in \code{step}. If \code{stratum} is
    \code{NULL}, \code{step} is assumed unstratified.
  }
}

\value{A matrix of the same dimension as \code{step}.}

\seealso{\code{jump2step}}

\examples{
}
