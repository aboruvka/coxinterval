\name{coxic}
\alias{coxic}

\title{Cox model for a Markov illness-death process under dual censoring}

\description{
  Fit a Cox model to a progressive Markov illness-death process observed
  under right-censored survival times and interval- or right-censored
  progression times.
}

\usage{
coxic(formula, data = parent.frame(), subset, init = NULL, formula.coxph = NULL, init.coxph = FALSE, control, ...)
}

\arguments{
  \item{formula}{
    a formula object of the general form \code{response ~ predictors},
    where \code{response} is a \code{"counting"}-type survival object
    returned by the \code{Surv} function and \code{predictors} contain
    clustering and state transition terms identified by the
    \code{cluster} and \code{trans} functions, respectively.
  }
  \item{data}{
    an optional data frame in which to interpret the variables in
    \code{formula} and \code{formula.coxph}.
  }
  \item{subset}{
    expression specifying which rows of \code{data} should be used
    in the fit. All observations are included by default.
  }
  \item{init}{
    a list of the vector \code{coef}, specifying the initial coefficient
    values, and data frame \code{basehaz}, giving the initial cumulative
    baseline transition intensities at the time partition ultimately
    defined by the \code{coxic.control} parameters \code{sieve.const}
    and \code{sieve.rate}. By default \code{coef} is zero and
    \code{basehaz} corresponds to linear functions of time with an upper
    bound of one.
  }
  \item{formula.coxph}{
    a list of formula objects specifying models to fit with \code{coxph}
    using singly right-censored observations in \code{data}. Use the
    shorthand \code{.} to indicate the same \code{predictors} given in
    \code{formula}, with the function \code{strata} in place by
    \code{trans}. Under dual-right--censoring, the first component of
    \code{formula.coxph} defaults to same model in \code{formula}, which
    is fit to observations singly-right--censored at the earlier
    censoring time.
  }
  \item{init.coxph}{
    a logical value indicating that \code{init} should be overrided by
    estimates based on the \code{coxph} fit to the first model in
    \code{formula.coxph}.
  }
  \item{control}{
    a list returned by the function \code{coxic.control} that defaults
    to \code{coxic.control()}.
  }
  \item{\dots}{
    additional arguments to be passed to \code{\link{coxic.control}}.
  }
}

\details{
  Missing values in \code{data} are handled by \code{\link{na.coxic}}.
}

\value{
  \item{call}{
  }
  \item{censor}{
  }
  \item{n}{
  }
  \item{m}{
  }
  \item{p}{
  }
  \item{coef}{
  }
  \item{var}{
  }
  \item{basehaz}{
  }
  \item{init}{
  }
  \item{loglik}{
  }
  \item{iter}{
  }
  \item{fenchel}{
  }
  \item{maxnorm}{
  }
  \item{cputime}{
  }
  \item{fit.coxph}{
  }
  \item{na.action}{
  }
  \item{censor.rate}{
  }
  \item{control}{
  }
}

\references{
  Boruvka, A. and Cook, R. J. (2014)
  Sieve estimation in a Markov illness-death process under dual censoring.
}

\seealso{
  \code{\link{cluster}}, \code{\link{Surv}}, \code{\link{trans}}
}

\examples{
# Fit Cox model to dual-right--censored data
fit <- coxic(Surv(start,stop,status) ~ cluster(id) + trans(from,to) +
I(z*(to==1)) + I(z*(from \%in\% 0 & to==2)) + I(z*(from \%in\% c(NA,1) & to==2)), data=dualrc, sieve.rate=2/5)
fit
par(mfrow=c(1,3))
by(fit$basehaz, fit$basehaz$trans, function(x) plot(x[,1:2], type="s",
main=paste(x[1,3]), xlim=c(0,4), ylim=c(0,2)))

# Fit Cox model to data with interval-censored progression times
fit <- coxic(Surv(start,stop,status) ~ cluster(id) + trans(from,to) +
I(z*(to==1)) + I(z*(from \%in\% 0 & to==2)) + I(z*(from \%in\% c(NA,1) & to==2)), data=dualic)
fit
par(mfrow=c(1,3))
by(fit$basehaz, fit$basehaz$trans, function(x) plot(x[,1:2], type="s",
main=paste(x[1,3]), xlim=c(0,4), ylim=c(0,2)))
}

\keyword{survival}
