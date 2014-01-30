\name{frame.assign}
\alias{frame.assign}

\title{Correspondence between the model frame and the model matrix}

\description{
  A utility function for \code{\link{coxaalenic}} and
  \code{\link{coxic}} used to identify which variables in a given
  \code{\link{model.frame}} object give rise to the columns in the
  corresponding \code{\link{model.matrix}}.
}

\usage{
frame.assign(frame, terms = NULL, matrix = NULL)
}

\arguments{
  \item{frame}{
    an object returned by \code{model.frame}
  }
  \item{terms}{
    the \code{"terms"} attribute of a \code{model.frame}
    object. Defaults to \code{attr(frame, "terms")}.
  }
  \item{matrix}{
    an object returned by \code{model.matrix}. Defaults to
    \code{model.matrix(terms, frame)}.
  }
}

\value{
  A data frame with two variables:
  \item{frame}{
    column index in the argument \code{frame},
  }
  \item{matrix}{
    corresponding column index in the argument \code{matrix}.
  }
}

\examples{
mf <- model.frame(Surv(left, right, type = "interval2")
                  ~ prop(treat), data = cosmesis)
mt <- attr(mf, "terms")
mm <- model.matrix(mt, mf)
mf[1:5, ]
mm[1:5, ]
frame.assign(mf, mt, mm)
}

\keyword{survival}
