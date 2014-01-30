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
    intercept term, \code{w} should have zero columns.
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
data(cosmesis)
fit <- coxaalenic(Surv(left, right, type = "interval2") ~ prop(treat),
                  data = cosmesis)
A <- coxaalenic.ineq(matrix(numeric(0), fit$n), nrow(fit$basehaz))
# Entries should be greater than or equal to zero
A \%*\% fit$basehaz[, 2]
}

\keyword{survival}
