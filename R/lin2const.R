### convert piecewise linear values to piecewise constant values
lin2const <- function(lin, time = "time", strata = "strata")
{
  if (is.character(strata)) strata <- match(strata, colnames(lin))
  if (is.na(strata)) strata <- NULL
  if (is.character(time)) time <- match(time, colnames(lin))
  m <- ncol(lin[, -c(time, strata), drop = FALSE])
  f <- function(x) c(0, -x[-length(x)]) + x
  g <- function(x) apply(x, 2, f)
  if (!is.null(strata)) {
    jump <- by(lin[, -c(time, strata), drop = FALSE], lin[, strata],
               g, simplify = FALSE)
    jump <- do.call(rbind, lapply(jump, matrix, ncol = m))
    len <- by(lin[, time, drop = FALSE], lin[, strata], g, simplify = FALSE)
    len <- do.call("rbind", lapply(len, matrix, ncol = m))
  }
  else {
    jump <- g(lin[, -time, drop = FALSE])
    len <- g(lin[, time])
  }
  const <- jump
  const[len > 0] <- const[len > 0] / len[len > 0]
  const <- cbind(const, lin[, c(time, strata)])
  rownames(const) <- rownames(lin)
  colnames(const) <- colnames(lin)
  if (is.data.frame(lin)) data.frame(const)
  else const
}
