### convert cumulative values to a step function
step2stepfun <- function(step, time = "time", strata = "strata", ...) {
  if (is.character(strata)) strata <- match(strata, colnames(step))
  if (is.na(strata)) strata <- NULL
  if (is.character(time)) time <- match(time, colnames(step))
  f <- function(x, y) stepfun(y[-1], x, ...)
  g <- function(x) apply(x[if ((n <- nrow(x)) > 1) 1:n else rep(1, 2),
                           -c(time, strata), drop = FALSE], 2, f,
                         y = c(if (n > 1) NULL else 0, x[, time]))
  if (!is.null(strata))
    fun <- unlist(by(step, step[, strata], g, simplify = FALSE))
  else fun <- g(step)
  fun
}
