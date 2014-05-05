\name{coxic.data}
\alias{coxic.data}

\title{Format dual-censored data for estimation routine}

\description{
  A utility function that formats data passed to \code{\link{coxic}} for
  the estimation routine.  This is made user-accessible to allow for
  examination of potential sieve sizes.  However note that
  \code{\link{coxic}} may omit observations depending on the \code{NA}
  action and its \code{subset} argument.
}

\usage{
coxic.data(id, start, stop, from, to, status, z, states)
}

\arguments{
  \item{id}{
    vector for the subject identifier, passed to \code{\link{coxic}} as
    the special \code{\link[survival]{cluster}} term in the
    \code{formula} argument
  }
  \item{start}{
    vector for the start time of the at-risk interval, passed to the
    \code{\link[survival]{Surv}} function in the response term of 
    \code{\link{coxic}}'s \code{formula} argument
  }
  \item{stop}{
    vector for the stop time of the at-risk interval, passed to the
    \code{\link[survival]{Surv}} function in the response term of
    \code{\link{coxic}}'s \code{formula} argument    
  }
  \item{from}{
    vector for the originating state in the transition type, passed as
    the first argument to \code{\link{coxic}} in the special
    \code{\link{trans}} \code{formula} term
  }
  \item{to}{
    vector for the originating state in the transition type, passed as
    the second argument to \code{\link{coxic}} in the special
    \code{\link{trans}} \code{formula} term
  }
  \item{status}{
    vector for the event status at the end of the at-risk interval,
    passed to the \code{\link[survival]{Surv}} function in the response
    term of \code{\link{coxic}}'s \code{formula} argument
  }
  \item{z}{
    matrix, passed as the covariate terms in \code{\link{coxic}}'s
    \code{formula} argument
  }
  \item{states}{
    vector of three entries giving the initial, intermediate and
    terminal state values found in \code{from} and \code{to}
  }
}

\value{
  \item{supp}{
    named list giving the support points for the progression (\code{t01}),
    progression-free survival (\code{t02}) and
    survival-after-progression (\code{t12}) times assigned by the
    semiparametric maximum likelihood estimator based on the subsample
    with known progression status
  }
  \item{left}{
    left endpoint of the potential censoring interval for the
    progression time
  }
  \item{right}{
    right endpoint of the potential censoring interval for the
    progression time
  }
  \item{u}{
    smallest observation times
  }
  \item{v}{
    largest observation times
  }
  \item{contrib}{
    likelihood contribution type: \code{contrib = 0} corresponds to
    negative progression status at \code{v}, \code{contrib = 1} to
    positive status, and \code{contrib = 2} to unknown status
  }
  \item{absorb}{
    survival status at \code{v}
  }
  \item{z}{
    \code{z} expanded to give transition-type--specific covariates
  }
}

\seealso{
  \code{\link{coxic}}
}

\examples{
with(subset(dualrc, id \%in\% 1:10),
     coxic.data(id, start, stop, from, to, status, z, 0:2))
}

\keyword{survival}
