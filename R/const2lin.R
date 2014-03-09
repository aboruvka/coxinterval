### accumulate values from piecewise constant function of time
const2lin <- function(const, time = NULL, stratum = NULL)
{
  f <- function(x) c(0, -x[-length(x)]) + x
  g <- function(x) if (length(dim(x))) apply(x, 2, f) else f(x)
  h <- function(x) if (length(dim(x))) apply(x, 2, cumsum) else cumsum(x)
  if (!is.null(stratum)) {
    if (is.null(time)) time <- stratum - 1
    m <- ncol(const) - 2
    len <- by(const[, time], const[, stratum], g, simplify = FALSE)
    len <- do.call(rbind, lapply(len, matrix, ncol = max(1, m)))
    lin <- by(const[, -c(time, stratum)] * len, const[, stratum], h,
              simplify = FALSE)
    lin <- do.call(rbind, lapply(lin, matrix, ncol = max(1, m)))
  }
  else {
    if (is.null(time)) time <- ncol(const)
    len <- g(const[, time])
    lin <- cumsum(const[, 1] * len)
  }
  lin <- cbind(lin, const[, c(time, stratum)])
  rownames(lin) <- rownames(const)
  colnames(lin) <- colnames(const)
  lin
}
