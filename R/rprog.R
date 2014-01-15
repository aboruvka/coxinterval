### generate data from a Markov illness-death process with Weibull baseline
### hazards and Cox-type covariate effects
rprog <- function(n, type.coef = c(rep(c(1, 1, 0.5), 2), c(1, 1, 1)),
                  shape = c(4/5, 1, 5/4), scale = c(1, 3/4, 3/2), ...) {
  type.coef <- matrix(type.coef, 3, 3)
  r <- rsurv(n, coef = type.coef[, 1], shape = shape[1], scale = scale[1],
             aalen = FALSE, ...)
  d <- r$data
  tau <- r$tau
  grid <- r$grid
  z <- as.matrix(subset(d, select = z1:z3))
  type.coef <- log(type.coef)
  t02 <- (-log(runif(n)) / exp(z %*% type.coef[, 2])
          + (scale[2] * d$trun)^shape[2])^(1/shape[2]) / scale[2]
  s <- pmin(d$time, t02)
  t <- s
  z <- matrix(z[s < t02, ], ncol = ncol(z))
  u <- runif(n)
  ## ABGK theorem II.6.7
  if (nrow(z) > 0)
    t[s < t02] <- (-log(u[s < t02]) / exp(z %*% type.coef[, 3])
                   + (scale[3] * s[s < t02])^shape[3])^(1/shape[3]) / scale[3]
  names(d)[names(d) %in% c("time", "event")] <- c("s", "t")
  d$s <- s
  d$t <- t
  if (!is.null(grid)) {
    m <- min(100, n * tau / 2)
    u <- (1:m) / m * tau
    grid <- cbind(u, mapply(function(x, y) (x * u)^y, x = scale, y = shape))
    colnames(grid) <-
      c("time", paste("hazard",  c("01", "02", "12"), sep = "."))
  }
  list(data = d, type.coef = type.coef, shape = shape, scale = scale,
       trun.coef = r$trun.coef, trun.rate = r$trun.rate, tau = tau, grid = grid)
}
