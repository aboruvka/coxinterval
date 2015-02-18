\name{maximalint}

\title{Find maximal intersections}

\description{Returns maximal intersections for a given interval-type
  Surv object}
\usage{
maximalint(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (x) 
{
    if (is.null(nrow(x))) 
        x <- matrix(x, nrow = 1)
    if (ncol(x) == 2) 
        x[is.na(x[, 2]), 2] <- Inf
    else if (ncol(x) > 2) {
        x[x[, 3] == 0, 2] <- Inf
        x[x[, 3] == 1, 2] <- x[x[, 3] == 1, 1]
    }
    ind <- x[, 1] == x[, 2]
    if (sum(!ind)) {
        l <- unique(x[!ind, 1])
        r <- r.minus <- unique(x[!ind, 2])
        if (max(l, r[r < Inf]) <= 2) 
            delta <- .Machine$double.eps
        else {
            s <- sort(unique(c(l, r)))
            if (length(s) > 1) 
                delta <- min(s[-1] - s[-length(s)])/10
            else delta <- s/10
        }
        r.minus[r \%in\% l] <- r[r \%in\% l] - delta
    }
    else l <- r <- r.minus <- NULL
    if (sum(ind)) {
        x[ind, 1] <- x[ind, 2] - .Machine$double.eps
        l <- c(l, unique(x[ind, 1]))
        r.minus <- c(r.minus, unique(x[ind, 2]))
        r <- c(r, unique(x[ind, 2]))
    }
    s <- rbind(cbind(l, l, 0), cbind(r.minus, r, 1))
    s <- s[order(s[, 1]), ]
    s <- cbind(s, rbind(s[-1, ], NA))
    s <- s[s[, 3] == 0 & s[, 6] == 1, c(1, 4, 5)]
    if (is.null(nrow(s))) 
        s <- matrix(s, nrow = 1)
    colnames(s) <- c("left", "right.noties", "right")
    if (nrow(s) < 2) 
        i <- matrix(1)
    else i <- t(apply(x[, 1:2], 1, function(x) 1 * (s[, 1] >= 
        x[1] & s[, 2] <= x[2])))
    list(int = s, ind = i)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
