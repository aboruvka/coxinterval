\name{na.coxic}
\alias{na.coxic}
\alias{na.coxic.default}

\title{Custom NA action for dual-censored data}

\description{
  An \code{NA} action for use with \code{\link{coxic}} that retains
  missing values in the first variable passed to the mandatory
  \code{\link{trans}} model term. All other missing values are filtered
  in the same manner as \code{\link[stats]{na.omit}}.
}

\usage{\method{na}{coxic}(object, ...)}

\arguments{
  \item{object}{
    an R object, typically the model frame passed to \code{\link{coxic}}
  }
  \item{\dots}{
    further arguments special methods could require.
  }
}

\value{
  Information on which observations were omitted due to \code{NA}
  values, or \code{NULL} if all observations were retained.
}

\seealso{\code{\link{coxic}}, \code{\link[stats]{na.action}}}

\examples{
mf <- model.frame(Surv(start, stop, status) ~ cluster(id)
                  + trans(from, to) + z, data = dualrc,
                  na.action = na.coxic)
attr(mf, "na.action")
}

\keyword{survival}
