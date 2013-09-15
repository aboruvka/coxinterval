### linear approximation
linapprox <- function(xyin, xout) {
  xyin <- xyin[xyin[, 1] > 0 & xyin[, 2] > 0, ]
  x <- sort(unique(xyin[, 1]))
  y <- as.vector(tapply(xyin[, 2], xyin[, 1], max))
  n <- length(x)
  xmax <- max(xout)
  if (max(x) < xmax) {
    x <- c(x, xmax)
    y <- c(y, y[n] + (y[n] - y[n-1])/(x[n] - x[n-1]) * (xmax - x[n-1]))
  }
  approx(c(0, x), c(0, y), xout)$y
}
