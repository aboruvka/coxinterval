### accumulate values from piecewise constant function of time
const2lin <- function(const, time = "time", strata = "strata")
{
  if (is.character(strata)) strata <- match(strata, colnames(const))
  if (is.na(strata)) strata <- NULL
  if (is.character(time)) time <- match(time, colnames(const))
  m <- ncol(const[, -c(time, strata), drop = FALSE])
  f <- function(x) c(0, -x[-length(x)]) + x
  g <- function(x) apply(x, 2, f)
  h <- function(x) apply(x, 2, cumsum)
  if (!is.null(strata)) {
    len <- by(const[, time, drop = FALSE], const[, strata], g, simplify = FALSE)
    len <- do.call("rbind", lapply(len, matrix, ncol = m))
    lin <- by(const[, -c(time, strata), drop = FALSE] * len, const[, strata],
              h, simplify = FALSE)
    lin <- do.call("rbind", lapply(lin, matrix, ncol = m))
  }
  else {
    len <- g(const[, time])
    lin <- h(const[, -time, drop = FALSE] * len)
  }
  lin <- cbind(lin, const[, c(time, strata)])
  rownames(lin) <- rownames(const)
  colnames(lin) <- colnames(const)
  if (is.data.frame(const)) data.frame(lin)
  else lin
}
