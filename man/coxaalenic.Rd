\name{coxaalenic}
\alias{coxaalenic}

\title{Cox-Aalen model for interval-censored survival data}

\description{
  Fit a Cox-Aalen model to interval-censored survival data with fixed
  covariates.
}

\usage{
coxaalenic(formula, data = parent.frame(), subset, init = NULL,
           formula.coxph = NULL, init.coxph = FALSE,
           close.cplex = FALSE, control, ...)
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
  \item{formula.coxph}{
  }
  \item{init.coxph}{
  }
  \item{close.cplex}{
  }
  \item{control}{
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
  Boruvka, A. and Cook, R. J. (2013)
  A Cox-Aalen model for interval-censored data.

  Martinussen, T. and Scheike, T. H. (2006)
  \emph{Dynamic Regression Models for Survival Data.}
  Springer.

  Scheike, T. H. and Zhang, M.-J. (2002)
  An additive-multiplicative Cox-Aalen regression model.
  \emph{Scandinavian Journal of Statistics}, \bold{29}, 75--88.
}

\seealso{\code{\link{prop}}, \code{\link{Surv}}}

\examples{
# Fit a Cox model to the breast cosmesis dataset
fit <- coxaalenic(Surv(left, right, type = "interval2") ~ prop(treat),
                  data = cosmesis)
fit
plot(fit$basehaz, type = "s")

# Fit a Cox-Aalen model to the tooth emergence dataset
fit <- coxaalenic(Surv(left, right, type = "interval2") ~ sex + prop(dmf),
                  data = tooth24)
fit
par(mfrow=c(1, 2))
plot(fit$basehaz[, -3], type = "s", main = "Cumulative baseline hazard")
plot(fit$basehaz[, -2], type = "s", main = "Cumulative coefficient")
}

\keyword{survival}
