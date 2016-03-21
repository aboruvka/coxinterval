### convert jumps to cumulative values
jump2step <- function(jump, time = "time", strata = "strata")
{
  if (is.character(strata)) strata <- match(strata, colnames(jump))
  if (is.na(strata)) strata <- NULL
  if (is.character(time)) time <- match(time, colnames(jump))
  m <- ncol(jump[, -c(time, strata), drop = FALSE])
  f <- function(x) apply(x, 2, cumsum)
  if (!is.null(strata)) {
    step <- by(jump[, -c(time, strata), drop = FALSE], jump[, strata],
               f, simplify = FALSE)
    step <- do.call("rbind", lapply(step, matrix, ncol = m))
  }
  else step <- f(jump[, -time, drop = FALSE])
  step <- cbind(step, jump[, c(time, strata)])
  rownames(step) <- rownames(jump)
  colnames(step) <- colnames(jump)
  if (is.data.frame(jump)) data.frame(step)
  else step
}
