\name{coxaalenic.ineq}
\alias{coxaalenic.ineq}

\title{Linear constraint matrix for the Cox-Aalen model}

\description{
  A utility function for \code{\link{coxaalenic}} to construct the
  linear constraint that must be satisfied by the cumulative regression
  function.
}

\usage{coxaalenic.ineq(w, k)}

\arguments{
  \item{w}{
    a matrix of covariate values. If the model contains only the
    intercept term, \code{ncol(w)} should be zero.
  }
  \item{k}{
    an integer giving the number of time points to be used in the model
    fit.
  }
}

\value{
  A block diagonal matrix
}

\examples{
fit <- coxaalenic(Surv(left, right, type = "interval2") ~ prop(treat),
                  data = cosmesis)
A <- coxaalenic.ineq(matrix(numeric(0), fit$n), nrow(fit$basehaz))
min(A \%*\% fit$basehaz[, 2]) # result should be nonnegative
}

\keyword{survival}
