### convert cumulative values to jumps
step2jump <- function(step, time = NULL, by = NULL) {
  f <- function(x) c(0, -x[-length(x)]) + x
  g <- function(x) if (length(dim(x))) apply(x, 2, f) else f(x)
  if (!is.null(by)) {
    if (is.null(time)) time <- by - 1
    m <- ncol(step) - 2
    jump <- by(step[, -c(time, by)], step[, by], g, simplify = FALSE)
    jump <- do.call(rbind, lapply(jump, matrix, ncol = max(1, m)))
  }
  else {
    if (is.null(time)) time <- ncol(step)
    jump <- g(step[, -time])
  }
  jump <- cbind(jump, step[, c(time, by)])
  rownames(jump) <- rownames(step)
  colnames(jump) <- colnames(step)
  jump
}
