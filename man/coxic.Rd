\name{coxic}
\alias{coxic}

\title{Fit a Cox model to an interval-censored Markov illness-death process}

\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}

\usage{
coxic(formula, data = parent.frame(), subset, init = NULL, rcprog = NULL, rcinit = FALSE, control, ...)
}

\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{subset}{
%%     ~~Describe \code{subset} here~~
}
  \item{init}{
%%     ~~Describe \code{init} here~~
}
  \item{rcprog}{
%%     ~~Describe \code{rcprog} here~~
}
  \item{rcinit}{
%%     ~~Describe \code{rcinit} here~~
}
  \item{control}{
%%     ~~Describe \code{control} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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

\seealso{
  \code{\link{cluster}}, \code{\link{Surv}}, \code{\link{trans}}
}

\examples{
fit <- coxic(Surv(start,stop,status) ~ cluster(id) + trans(from,to) +
I(z*(to==1)) + I(z*(from \%in\% 0 & to==2)) + I(z*(from \%in\% c(NA,1) & to==2)), data=dualrc, sieve.rate=2/5)
fit
par(mfrow=c(1,3))
by(fit$basehaz, fit$basehaz$trans, function(x) plot(x[,1:2], type="s",
main=paste(x[1,3]), xlim=c(0,4), ylim=c(0,2)))
}

\keyword{survival}
