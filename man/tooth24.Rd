\encoding{UTF-8}
\name{tooth24}
\alias{tooth24}
\docType{data}

\title{Permanent tooth emergence data}

\description{
  Interval-censored times to emergence of the permanent maxillary left
  first premolar (tooth 24) among Belgian school children participating
  in the Signal Tandmobiel® study.
}

\usage{data(tooth24)}

\format{
  A data frame with 4386 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{
      child identifier.
    }
    \item{\code{left}}{
      left-endpoint of the censoring interval for time to emergence.
    }
    \item{\code{right}}{
      right-endpoint of the censoring interval for time to
      emergence. Following the \code{\link{Surv}} object format under
      \code{type = "interval2"} censoring, the \code{NA} value
      corresponds to a right-censored observation at the time given by
      the variable \code{left}.
    }
    \item{\code{sex}}{
      sex of the child: \code{sex = 0} corresponds to male,
      \code{sex = 1} to female. 
    }
    \item{\code{dmf}}{
      status of permanent tooth 24's primary predecessor: \code{dmf = 0}
      is sound; \code{dmf = 1} indicates decayed, missing due to caries,
      or filled.
    }
  }
}

\source{
  \url{http://grass.upc.edu/software/tooth24}
}

\references{
  Gómez, G., Calle, M. L., Oller, R. and Langohr, K. (2009)
  Tutorial on methods for interval-censored data and their
  implementation in R.
  \emph{Statistical Modelling}, \bold{9}, 259--297.

  Vanobbergen, J., Martens, L., Lesaffre, E. and Declerck, D (2000)
  The Signal-Tandmobiel® project -- A longitudinal intervention health
  promotion study in Flanders (Belgium): Baseline and first year
  results.
  \emph{European Journal of Paediatric Dentistry}, \bold{2}, 87--96.
}

\examples{data(tooth24)}

\keyword{datasets}
