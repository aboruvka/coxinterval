### convert cumulative values to jumps
step2jump <- function(step, time = "time", strata = "strata")
{
  if (is.character(strata)) strata <- match(strata, colnames(step))
  if (is.na(strata)) strata <- NULL
  if (is.character(time)) time <- match(time, colnames(step))
  m <- ncol(step[, -c(time, strata), drop = FALSE])
  f <- function(x) c(0, -x[-length(x)]) + x
  g <- function(x) apply(x, 2, f)
  if (!is.null(strata)) {
    jump <- by(step[, -c(time, strata), drop = FALSE], step[, strata],
               g, simplify = FALSE)
    jump <- do.call("rbind", lapply(jump, matrix, ncol = m))
  }
  else jump <- g(step[, -time, drop = FALSE])
  jump <- cbind(jump, step[, c(time, strata)])
  rownames(jump) <- rownames(step)
  colnames(jump) <- colnames(step)
  if (is.data.frame(step)) data.frame(jump)
  else jump
}
