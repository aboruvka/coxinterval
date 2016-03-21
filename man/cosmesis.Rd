\name{cosmesis}
\alias{cosmesis}
\docType{data}

\title{Breast cosmesis data}

\description{
  Interval-censored times to cosmetic deterioration for breast cancer
  patients undergoing radiation or radiation plus chemotherapy.
}

\usage{data(cosmesis)}

\format{
  A data frame with 94 observations on the following 3 variables.
  \describe{
    \item{\code{left}}{
      left endpoint of the censoring interval in months
    }
    \item{\code{right}}{
      right endpoint of the censoring interval in months
    }
    \item{\code{treat}}{
      a factor with levels \code{RT} and \code{RCT} representing
      radiotherapy-only and radiation plus chemotherapy treatments,
      respectively
    }
  }
  From Huang and Weller (1995) the maximum likelihood estimate of the
  Cox model regression coefficient for this data set is 0.797.  Its
  standard error based on the curvature of the profile-likelihood is
  0.29.  Similar results are obtained with \code{\link{coxaalen}}.
}

\source{
  Finkelstein, D. M. and Wolfe, R. A. (1985)
  \href{http://www.jstor.org/stable/2530698}{
    A semiparametric model for regression analysis of interval-censored
    failure time data}.
  \emph{Biometrics} \bold{41}, 933--945.
}

\references{
  Finkelstein, D. M. (1986)
  \href{http://www.jstor.org/stable/2530698}{
    A proportional hazards model for interval-censored failure time data}.
  \emph{Biometrics} \bold{42}, 845--854.

  Huang, J. and Wellner, J. A. (1995)
  \href{http://www.stat.washington.edu/research/reports/1995/tr290.pdf}{
    Efficient estimation for the proportional hazards model with 'case
    2' interval censoring}.
  Technical report 290. Department of Statistics, University of
  Washington.
}

\seealso{
  \code{\link{coxaalen}}
}

\examples{data(cosmesis)}

\keyword{datasets}
