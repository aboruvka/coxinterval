### convert piecewise linear values to piecewise constant values
lin2const <- function(lin, time = NULL, stratum = NULL) {
  f <- function(x) c(0, -x[-length(x)]) + x
  g <- function(x) if (length(dim(x))) apply(x, 2, f) else f(x)
  if (!is.null(stratum)) {
    if (is.null(time)) time <- stratum - 1
    m <- ncol(lin) - 2
    jump <- by(lin[, -c(time, stratum)], lin[, stratum], g, simplify = FALSE)
    jump <- do.call(rbind, lapply(jump, matrix, ncol = max(1, m)))
    len <- by(lin[, time], lin[, stratum], g, simplify = FALSE)
    len <- do.call(rbind, lapply(len, matrix, ncol = max(1, m)))
  }
  else {
    if (is.null(time)) time <- ncol(lin)
    jump <- g(lin[, -time])
    len <- g(lin[, time])
  }
  const <- jump
  const[len > 0] <- const[len > 0] / len[len > 0]
  const <- cbind(const, lin[, c(time, stratum)])
  rownames(const) <- rownames(lin)
  colnames(const) <- colnames(lin)
  const
}
