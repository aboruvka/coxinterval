\name{coxaalenic.ineq}
\alias{coxaalenic.ineq}

\title{Linear constraint matrix for the Cox-Aalen model}

\description{
  A utility function for \code{\link{coxaalenic}} used to construct the
  linear constraint that must be satisfied by the cumulative regression
  function.
}

\usage{coxaalenic.ineq(w, k)}

\arguments{
  \item{w}{
    a matrix of covariate values.  If the model contains only the
    intercept term, \code{ncol(w)} should be zero.
  }
  \item{k}{
    an integer giving the number of time points to be used in the model
    fit.
  }
}

\value{
  The block diagonal matrix in the matrix expression for the linear
  constraint (Boruvka and Cook, 2014a).
}

\references{
  Boruvka, A. and Cook, R. J. (2014a)
  A Cox-Aalen model for interval-censored data.
}

\examples{
fit <- coxaalenic(Surv(left, right, type = "interval2") ~ prop(treat),
                  data = cosmesis)
A <- coxaalenic.ineq(matrix(numeric(0), fit$n), nrow(fit$basehaz))
min(A \%*\% fit$basehaz[, 2]) # result should be nonnegative
}

\keyword{survival}
