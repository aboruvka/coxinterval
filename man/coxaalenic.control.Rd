\name{coxaalenic.control}
\alias{coxaalenic.control}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Control Cox-Aalen model fit}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
coxaalenic.control(eps = 1e-07, iter.max = 5000, step.frac = 1/2, step.scale = 1/3, coef.typ = 1, coef.max = 10, trace = FALSE, thread.max = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eps}{
%%     ~~Describe \code{eps} here~~
}
  \item{iter.max}{
%%     ~~Describe \code{iter.max} here~~
}
  \item{step.frac}{
%%     ~~Describe \code{step.frac} here~~
}
  \item{step.scale}{
%%     ~~Describe \code{step.scale} here~~
}
  \item{coef.typ}{
%%     ~~Describe \code{coef.typ} here~~
}
  \item{coef.max}{
%%     ~~Describe \code{coef.max} here~~
}
  \item{trace}{
%%     ~~Describe \code{trace} here~~
}
  \item{thread.max}{
%%     ~~Describe \code{thread.max} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (eps = 1e-07, iter.max = 5000, step.frac = 1/2, step.scale = 1/3, 
    coef.typ = 1, coef.max = 10, trace = FALSE, thread.max = 1) 
{
    if (eps <= .Machine$double.eps) 
        stop("Invalid epsilon. Choose a small value > ", .Machine$double.eps, 
            ".")
    if (iter.max < 0) 
        stop("Invalid maximum iterations. Choose a large positive integer.")
    if (coef.typ < eps) 
        stop("Invalid coefficient magnitude. Choose a positive value.")
    if (coef.max <= coef.typ) 
        stop("Invalid maximum coefficient size. Choose a value > ", 
            coef.typ, ".")
    if (step.frac < eps | step.frac > 1) 
        stop("Invalid step fraction. Choose a value in (0, 1).")
    if (step.scale < eps | step.scale >= 1/2) 
        stop("Invalid step scale. Choose a value in (0, 1/2].")
    if (thread.max < 0 | thread.max > detectCores(logical = TRUE)) 
        stop(paste("Invalid maximum threads.", "Choose integer from zero to number of logical processors."))
    list(eps = eps, iter.max = iter.max, step.frac = step.frac, 
        step.scale = step.scale, coef.typ = coef.typ, coef.max = coef.max, 
        trace = trace, thread.max = thread.max)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
