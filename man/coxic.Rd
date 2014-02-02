\name{coxic}
\alias{coxic}
\alias{print.coxic}
\alias{na.coxic}
\alias{na.coxic.default}
\alias{na.coxic.data.frame}

\title{Cox model for a Markov illness-death process under dual censoring}

\description{
  Fit a Cox model to a progressive Markov illness-death process observed
  under right-censored survival times and interval- or right-censored
  progression times.
}

\usage{
coxic(formula, data = parent.frame(), subset, init = NULL,
      formula.coxph = NULL, init.coxph = FALSE, control, ...)
}

\arguments{
  \item{formula}{
    an expression of the form \code{response ~ terms}, where
    \code{response} is a survival object returned by the
    \code{\link[survival]{Surv}} function and \code{terms} contain
    clustering and state transition terms identified by the
    \code{\link[survival]{cluster}} and \code{\link{trans}} functions,
    respectively.
  }
  \item{data}{
    an optional data frame in which to interpret the variables in
    \code{formula} and \code{formula.coxph}.
  }
  \item{subset}{
    expression specifying which rows of \code{data} should be used
    in the fit.  All observations are included by default.
  }
  \item{init}{
    a list of the vector \code{coef}, specifying the initial coefficient
    values, and data frame \code{basehaz}, giving the initial cumulative
    baseline transition intensities at the time partition ultimately
    defined by the \code{coxic.control} parameters \code{sieve.const}
    and \code{sieve.rate}.  By default \code{coef} is zero and
    \code{basehaz} corresponds to linear functions of time with an upper
    bound of one.
  }
  \item{formula.coxph}{
    a list of formula objects specifying models to fit with
    \code{\link[survival]{coxph}} using singly right-censored
    observations in \code{data}.  The shorthand \code{.} indicates
    the same terms given in \code{formula}, with the function
    \code{\link[survival]{strata}} in place by
    \code{\link{trans}}.  Under dual-right--censoring, the first
    component of \code{formula.coxph} defaults to same model in
    \code{formula}, which is fit to observations singly-right--censored
    at the earlier censoring time.
  }
  \item{init.coxph}{
    a logical value indicating that \code{init} should be overrided by
    estimates based on the \code{\link[survival]{coxph}} fit to the
    first model in \code{formula.coxph}.
  }
  \item{control}{
    a named list of parameters controlling the model fit, returned by
    the function \code{\link{coxic.control}}.  This defaults to
    \code{coxic.control()}.
  }
  \item{\dots}{
    additional arguments to be passed to \code{\link{coxic.control}}.
  }
}

\details{
  A valid \code{formula} argument can be expressed as
  \preformatted{Surv(<start>, <end>, <event>) ~ cluster(<id>) +
  trans(<from>, <to>) + <covariate terms>} where \code{(<start>, <end>]}
  is largest \emph{known} time interval over which individual
  \code{<id>} is at risk for a transition between the states
  \code{<from>} and \code{<to>}.  The variable \code{<event>} indicates
  whether or not a transition is observed to occur at \code{<end>}.  The
  observation scheme may prevent us from "knowing" an at-risk interval
  with positive length.  In this case we use the convention
  \code{<start> = <end>}.

  Under dual censoring (Boruvka and Cook, 2014b), both the originating
  state for an at-risk interval may be unknown; this case is handled
  with \code{from = NA}, \code{<to>} equal to the index of the terminal
  state and any transition-type--specific covariates taking on the
  values assumed when \code{<from>} is equal to the intermediate state
  index.  The missing values are retained by the \code{NA} action
  \code{\link{na.coxic}}.  The default \code{NA} action is used to handle
  any missing values passed to \code{\link[survival]{coxph}}.

  A consequence of dual censoring is that any discrete maximum
  likelihood estimator has ambiguous support at any failure times
  associated with these \code{NA} values.  To resolve this, the
  cumulative baseline transition intensities restricted to piecewise
  linear functions over a sieve with size controlled by arguments passed
  to \code{\link{coxic.control}}.  This approach requires that both types
  of transitions to the terminal state are, at least for some subjects,
  observed exactly.
}

\value{
  An object of the class \code{"coxic"}, which is a list with the
  following components.
  \item{call}{
    the matched call to \code{coxic}.
  }
  \item{censor}{
    a string indicating the dual censoring type.  The value "right"
    corresponds to strictly dual right-censored data.  All other cases
    return "interval".
  }
  \item{n}{
    size of the sample used in the model fit.
  }
  \item{m}{
    number of at-risk intervals used in the model fit.
  }
  \item{p}{
    number of regression coefficients.
  }
  \item{coef}{
    a named \code{p} vector of regression coefficients.
  }
  \item{var}{
    a named \code{p} by \code{p} covariance matrix of the regression
    coefficients.
  }
  \item{basehaz}{
    a data frame giving the cumulative baseline transition intensities
    evaluated over the sieve.
  }
  \item{init}{
    list of initial values used in the model fit.
  }
  \item{loglik}{
    a vector giving the initial and final log-likelihood values.
  }
  \item{iter}{
    number of iterations needed to achieve the model fit.
  }
  \item{fenchel}{
    the inner product between the final parameter value and the score
    function.
  }
  \item{maxnorm}{
    the supremum norm between the penultimate and final parameter
    values.
  }
  \item{cputime}{
    the processing time used for parameter and variance estimation.
  }
  \item{fit.coxph}{
    an unnamed list whose components give the
    \code{\link[survival]{coxph}} fit to any models specified by the
    \code{formula.coxph} argument.
  }
  \item{na.action}{
    the \code{"na.action"} attribute of the model frame.  Here this
    corresponds to the result from the custom \code{NA} action
    \code{\link{na.coxic}}.
  }
  \item{censor.rate}{
    a named vector of censoring rates.
  }
  \item{control}{
    a list of arguments passed to \code{\link{coxic.control}}.
  }
}

\references{
  Boruvka, A. and Cook, R. J. (2014b)
  Sieve estimation in a Markov illness-death process under dual censoring.
}

\seealso{
  \code{\link[survival]{cluster}}, \code{\link{dualic}},
  \code{\link{dualrc}}, \code{\link[survival]{Surv}},
  \code{\link{trans}}
}

\examples{
# Fit Cox model to dual-right--censored data
fit <- coxic(Surv(start, stop, status) ~ cluster(id) + trans(from, to)
             + I(z * (to == 1)) + I(z * (from \%in\% 0 & to == 2))
             + I(z* (from \%in\% c(NA, 1) & to == 2)), data = dualrc,
             sieve.rate = 2/5)
fit
par(mfrow = c(1, 3))
by(fit$basehaz, fit$basehaz$trans, function(x) plot(x[, 1:2],
   type = "l", main = paste(x[1, 3]), xlim = c(0, 4), ylim = c(0, 2)))

# Fit Cox model to data with interval-censored progression times
fit <- coxic(Surv(start, stop, status) ~ cluster(id) + trans(from, to)
             + I(z *(to == 1)) + I(z * (from \%in\% 0 & to == 2))
             + I(z * (from \%in\% c(NA, 1) & to == 2)), data = dualic)
fit
par(mfrow=c(1, 3))
by(fit$basehaz, fit$basehaz$trans, function(x) plot(x[, 1:2],
   type = "l", main = paste(x[1, 3]), xlim = c(0, 4), ylim = c(0, 2)))
}

\keyword{survival}
