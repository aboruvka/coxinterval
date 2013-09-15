### convert jumps to cumulative values
jump2step <- function(jump, time = NULL, by = NULL) {
  f <- function(x) if (length(dim(x))) apply(x, 2, cumsum) else cumsum(x)
  if (!is.null(by)) {
    if (is.null(time)) time <- by - 1
    m <- ncol(jump) - 2
    step <- by(jump[, -c(time, by)], jump[, by], f, simplify = FALSE)
    step <- do.call(rbind, lapply(step, matrix, ncol = max(1, m)))
  }
  else {
    if (is.null(time)) time <- ncol(jump)
    step <- cumsum(jump[, 1])
  }
  step <- cbind(step, jump[, c(time, by)])
  rownames(step) <- rownames(jump)
  colnames(step) <- colnames(jump)
  step
}
