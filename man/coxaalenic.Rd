\name{coxaalenic}
\alias{coxaalenic}

\title{Cox-Aalen model for interval-censored data}

\description{
  Fits a Cox-Aalen model to interval-censored survival data with fixed
  covariates.
}

\usage{
coxaalenic(formula, data = parent.frame(), subset, init = NULL, rcsurv =
NULL, rcinit = FALSE, control, close.cplex = FALSE, ...)
}

\arguments{
  \item{formula}{
  }
  \item{data}{
  }
  \item{subset}{
  }
  \item{init}{
  }
  \item{rcsurv}{
  }
  \item{rcinit}{
  }
  \item{control}{
  }
  \item{close.cplex}{
  }
  \item{\dots}{
  }
}
\details{
}

\value{
  \item{call}{
  }
  \item{n}{
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
  \item{iter}{
  }
  \item{loglik}{
  }
  \item{fenchel}{
  }
  \item{maxnorm}{
  }
  \item{cputime}{
  }
  \item{rcfit}{
  }
  \item{na.action}{
  }
  \item{censor.rate}{
  }
  \item{control}{
  }
}

\references{
  Martinussen, T. and Scheike, T. H. (2006)
  \emph{Dynamic Regression Models for Survival Data.}
  Springer.

  Scheike, T. H. and Zhang, M.-J. (2002)
  An additive-multiplicative Cox-Aalen regression model.
  \emph{Scandinavian Journal of Statistics}, \bold{29}, 75--88.
}

\seealso{\code{\link{prop}}, \code{\link{Surv}}}

\examples{
# Fit a Cox model to the breast cosmesis data set
data(cosmesis)
fit <- coxaalenic(Surv(left, right, type='interval2') ~ prop(treat), data=cosmesis)
fit
plot(fit$basehaz, type="s")

}

\keyword{survival}
