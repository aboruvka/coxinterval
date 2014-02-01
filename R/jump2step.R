### convert jumps to cumulative values
jump2step <- function(jump, time = NULL, stratum = NULL) {
  f <- function(x) if (length(dim(x))) apply(x, 2, cumsum) else cumsum(x)
  if (!is.null(stratum)) {
    if (is.null(time)) time <- stratum - 1
    m <- ncol(jump) - 2
    step <- by(jump[, -c(time, stratum)], jump[, stratum], f, simplify = FALSE)
    step <- do.call(rbind, lapply(step, matrix, ncol = max(1, m)))
  }
  else {
    if (is.null(time)) time <- ncol(jump)
    step <- cumsum(jump[, 1])
  }
  step <- cbind(step, jump[, c(time, stratum)])
  rownames(step) <- rownames(jump)
  colnames(step) <- colnames(jump)
  step
}
