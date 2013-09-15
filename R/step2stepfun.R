### convert cumulative values to a step function
step2stepfun <- function(step, time = NULL, by = NULL) {
  f <- function(x, y) stepfun(y[-1], x)
  g <- function(x) {
    if (length(dim(x[, -time]))) apply(x[, -time], 2, f, y = x[, time])
    else f(x[, -time], x[, time])
  }
  if (!is.null(by)) {
    if (is.null(time)) time <- by - 1
    fun <- unlist(by(step[, -by], step[, by], g, simplify = FALSE))
  }
  else {
    if (is.null(time)) time <- ncol(step)
    fun <- g(step)
  }
  fun
}
