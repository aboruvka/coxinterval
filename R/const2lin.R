### convert piecewise constant values to piecewise linear values
const2lin <- function(const, time = NULL, by = NULL) {
  f <- function(x) c(0, -x[-length(x)]) + x
  g <- function(x) if (length(dim(x))) apply(x, 2, f) else f(x)
  h <- function(x) if (length(dim(x))) apply(x, 2, cumsum) else cumsum(x)
  if (!is.null(by)) {
    if (is.null(time)) time <- by - 1
    m <- ncol(const) - 2
    len <- by(const[, time], const[, by], g, simplify = FALSE)
    len <- do.call(rbind, lapply(len, matrix, ncol = max(1, m)))
    step <- by(const[, -c(time, by)] * len, const[, by], h, simplify = FALSE)
    step <- do.call(rbind, lapply(step, matrix, ncol = max(1, m)))
  }
  else {
    if (is.null(time)) time <- ncol(const)
    len <- g(const[, time])
    step <- cumsum(const[, 1] * len)
  }
  step <- cbind(step, const[, c(time, by)])
  rownames(step) <- rownames(const)
  colnames(step) <- colnames(const)
  step
}
