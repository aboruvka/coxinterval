\name{coxinterval.control}
\alias{coxinterval.control}

\title{Control model fit}

\description{
  Set parameters controlling the model fit.
}

\usage{
coxinterval.control(eps = 1e-7, iter.max = 5000, coef.typ = 1,
                    coef.max = 10, return.data = FALSE,
                    eps.norm = c("max", "grad"), armijo = 1/3,
                    var.coef = TRUE, trace = FALSE, thread.max = 1,
                    sieve = TRUE, sieve.const = 1, sieve.rate = 1/3,
                    risk.min = 1)
}

\arguments{
  \item{eps}{
    threshold value for the norm used to measure convergence in the
    parameter estimates.
  }
  \item{iter.max}{
    maximum number of iterations to attempt.  This ensures that
    the estimation routine will eventually exit, even when the
    convergence criteria are not met.
  }
  \item{coef.typ}{
    a scalar or vector of "typical" (absolute) values for the
    regression coefficient.
  }
  \item{coef.max}{
    a scalar or vector of probable upper bounds for the regression
    coefficient.  This and the \code{coef.typ} arguments tune
    variance estimation via the curvature in the profile
    log-likelihood, following Boruvka and Cook (2015, Section 6).
  }
  \item{return.data}{
    a logical value indicating that the model object returned should
    contain an element \code{data} that generally gives the assigned
    support points and model matrix.  Further details on the output is
    provided in the documentation for each model function.
  }
  \item{eps.norm}{
    a character string identifying the norm to use in the convergence
    criteria for \code{\link{coxaalen}}---either the maximum norm
    between the current and previous parameter values
    (\code{eps.norm = "max"}) or the absolute inner product between the
    current value and the score (\code{eps.norm = "grad"}).
  }
  \item{armijo}{
    a scale factor in (0, 1/2) for Armijo's (1966) rule---a line search
    used to ensure that each iteration in the estimation routine for
    \code{\link{coxaalen}} achieves an adequate increase in the
    log-likelihood.  The model fit is typically not very sensitive to
    this value.
  }
  \item{var.coef}{
    a logical value indicating that \code{\link{coxaalen}} standard
    errors for the multiplicative regression coefficients should be
    estimated.  This is done via profile likelihood---an approach that
    can require an inordinate amount of processing time under many
    regression coefficients and larger sample size.
  }
  \item{trace}{
    a logical value indicating that, on execution of
    \code{\link{coxaalen}}, CPLEX should print its results to the
    screen.
  }
  \item{thread.max}{
    maximum number of CPU threads for \code{\link{coxaalen}} to allocate
    to CPLEX.  The default value disables multithreading.  A value of
    zero allows CPLEX to set the number of threads automatically.  The
    actual number of threads used is limited by the number of available
    processors and the CPLEX license.
  }
  \item{sieve}{
    a logical value indicating that the sieve rather than the
    semiparametric maximum likelihood estimator should be fit by
    \code{\link{coxdual}}.  The default \code{TRUE} is recommended to
    avoid issues with support finding and convergence.
  }
  \item{sieve.const}{
    a constant factor that, in part, determines the sieve size.  The
    factor can be made specific to the transition type with
    \code{sieve.const} a vector of length three.  Indexing the states
    from zero, this vector's components correspond to the state 0 to
    state 1, 0 to 2, and 1 to 2 transition types, respectively.
  }
  \item{sieve.rate}{
    a scalar in (1/8, 1/2) determining the rate at which the sieve
    increases with the sample size.
  }
  \item{risk.min}{
    a positive integer giving the minimum size of risk set for support
    points defining the sieve.
  }
}

\value{
  A list of the above arguments with their final values.
}

\details{
  For a given sample size \emph{n}, the sieve for \code{\link{coxdual}}
  has size at most \code{sieve.const*}\emph{n}\code{^sieve.rate}.  Any
  reduction in size from this value is applied to ensure that each
  subinterval in the sieve's time partition captures at least one
  support point from the semiparametric maximum likelihood estimator
  based on the subsample with known progression status (Boruvka and
  Cook, 2016).
}

\references{
  Boruvka, A. and Cook, R. J. (2015)
  \href{http://dx.doi.org/10.1111/sjos.12113}{
    A Cox-Aalen model for interval-censored data}.
  \emph{Scandinavian Journal of Statistics} \bold{42}, 414--426.

  Boruvka, A. and Cook, R. J. (2016)
  \href{http://dx.doi.org/10.1093/biostatistics/kxv042}{
    Sieve estimation in a Markov illness-death process under dual
    censoring}.
  \emph{Biostatistics} \bold{17}, 350--363.

  Armijo, L. (1966)
  \href{http://dx.doi.org/10.2140/pjm.1966.16.1}{
    Minimization of functions having Lipschitz continuous first partial
    derivatives.}
  \emph{Pacific Journal of Mathematics} \bold{16}, 1--3.
}

\seealso{
  \code{\link{coxaalen}}, \code{\link{coxdual}}
}

\examples{
if (is.loaded("coxaalen", "coxinterval")) \{
  f <- Surv(left, right, type = "interval2") ~ prop(treat)
  coxaalen(f, data = cosmesis,
           control = coxinterval.control(iter.max = 2, trace = TRUE))
  coxaalen(f, data = cosmesis, iter.max = 2)
\}

f <- Surv(start, stop, status) ~ cluster(id) + strata(from, to) +
     I(z * (to == 1)) + I(z * (from \%in\% 0 & to == 2)) +
     I(z * (from \%in\% c(NA, 1) & to == 2))
coxdual(f, data = dualrc,
        control = coxinterval.control(eps = 1e-5, sieve.rate = 2/5))
coxdual(f, data = dualrc, eps = 1e-5, sieve.rate = 2/5)
}

\keyword{survival}
