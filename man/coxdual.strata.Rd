\name{coxdual.strata}
\alias{coxdual.strata}

\title{Identify transition type in model terms}

\description{
  A special function for \code{\link{coxdual}} that identifies formula
  terms specifying the state-transition type.
}

\usage{coxdual.strata(from, to)}

\arguments{
  \item{from}{
    a variable representing the originating state.
  }
  \item{to}{
    a variable representing the subsequent state.
  }
}

\value{
  A combination of the \code{from} and \code{to} arguments by column
  with the attribute \code{"states"}, a vector giving the unique
  non-missing values in the \code{from} and \code{to} arguments ordered
  so that the initial state appears first, the intermediate state
  second, and the terminal state last.
}

\seealso{
  \code{\link{coxdual}}
}

\examples{with(dualrc[1:10, ], coxdual.strata(from, to))}

\keyword{survival}
