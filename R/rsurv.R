### generate failure time from a Cox-type model with Weibull baseline hazard
rsurv <- function(n, coef = c(2, 2, 0.5), shape = c(3/2, 2/3), scale = c(1, 1),
                  trun = FALSE, trun.time = 0, trun.coef = c(1/4, 1, 1, 1),
                  trun.rate = 10, tau = 2, M = 1e10, aalen = TRUE,
                  grid = FALSE) {
  coef <- log(coef)
  ## W ~ (1, U[0, 1]), Z ~ NULL
  if (aalen) {
    w <- cbind(1, runif(n))
    z <- NULL
    coef <- coef[-1]
    trun.coef <- trun.coef[-2]
  }
  ## Z ~ U[0, 1]
  else {
    w <- matrix(1, nrow = n)
    z <- runif(n)
    shape <- shape[1]
    scale <- scale[1]
  }
  ## Z ~ (Z, N(0, 1), Bernoulli(0.5))
  z <- cbind(z, rnorm(n), sample(c(0, 1), n, replace = TRUE))
  colnames(z) <- paste("z", 1:(3 - aalen), sep = "")
  if (trun) {
    ## potential left-truncation times ~ exponential(trun.rate)
    trun.time <- rexp(n, trun.rate)
    ## left-truncated with prob p, logit(p) = t(1, z) %*% trun.coef
    trun.coef <- log(trun.coef)
    trun <- trun.coef[1] + z %*% trun.coef[-1]
    trun <- sapply(exp(trun) / (1 + exp(trun)), rbinom, n = 1, size = 1)
    trun.time[trun == 0] <- 0
  }
  else {
    ## preserve random number sequence
    #rexp(n, trun.rate)
    #trun <- trun.coef[1] + z %*% trun.coef[-1]
    #trun <- sapply(exp(trun) / (1 + exp(trun)), rbinom, n = 1, size = 1)
    #if (trun.time[1] == 0 & length(trun.time) == 1)
    trun.time <- rep(0, n)
    trun.coef <- trun.rate <- NULL
  }
  if (length(unique(shape)) == 1) {
    shape <- unique(shape)
    r <- -log(runif(n)) * exp(-z %*% coef)
    r <- (r + (w %*% scale^shape)*trun.time^shape) / (w %*% scale^shape)
    r <- cbind(r^(1/shape), 1)
    ind <- r[, 1] > M
    r[ind, 1] <- M
    r[ind, 2] <- 0
  }
  else {
    f <- function(x, t)
      exp(-x[1:2] %*% ((scale * t)^shape - (scale * x[3])^shape) * x[4]) - x[5]
    g <- function(x) {
      r <- try(uniroot(function(t) f(x, t), interval = c(0, M),
                       tol = .Machine$double.eps)$root, silent = TRUE)
      if (inherits(r, "try-error")) c(tau, 0)
      else c(r, 1)
    }
    r <- t(apply(cbind(w, trun.time, exp(z %*% coef), runif(n)), 1, g))
  }
  d <- data.frame(id = 1:n, trun = trun.time, time = r[, 1], event = r[, 2])
  if (aalen) d <- cbind(d, w = w[, -1])
  d <- cbind(d, z)
  if (grid) {
    m <- min(tau * 100, n * tau / 2)
    u <- (1:m) / m * tau
    grid <- cbind(u, mapply(function(x, y) (x * u)^y, x = scale, y = shape))
    if (aalen) colnames(grid) <- c("time", "intercept", "w")
    else colnames(grid) <- c("time", "hazard")
  }
  else grid <- NULL
  list(data = d, coef = coef, shape = shape, scale = scale,
       trun.coef = trun.coef, trun.rate = trun.rate, tau = tau, grid = grid)
}
