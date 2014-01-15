### generate interval-censored survival data from evenly-spaced inspection times
rsched <- function(data, k = 8, beta = c(1/9, 1, 1, 1), depend = FALSE,
                   shape = 3/4, tau = 2, kmax = 10) {
  n <- nrow(data)
  if (is.element("w", names(data))) {
    cv <- as.matrix(subset(data, select = c(w, z1, z2)))
    model <- "w + prop(z1) + prop(z2)"
  }
  else {
    cv <- as.matrix(subset(data, select = c(z1, z2, z3)))
    model <- "z1 + z2 + z3"
  }
  ## need k > 1
  k <- max(2, k)
  sd <- tau / (4 * (k + 1))
  a <- matrix(-2 * sd, nrow = n, ncol = k)
  b <- -a
  a[, 1] <- -4 * sd
  b[, k] <- tau
  u <- matrix(runif(n * kmax)[1:(n * k)], nrow = n, ncol = k)
  sched <- matrix(tau/(k + 1) * 1:k, nrow = n, ncol = k, byrow = TRUE)
  sched <- sched + matrix(data$trun, nrow = n, ncol = k)
  sched <- sched + qnorm(pnorm(a) + u * (pnorm(b) - pnorm(a)))
  ## administrative censoring
  cens <- rep(tau, n)
  ## kth inspection, k > 1, missed with prob p, logit(p) = t(1, cv) %*% beta
  beta <- log(beta)
  if (!depend | is.null(data$t)) {
    pred <- beta[1] + cv %*% beta[-1]
    seen <- sapply(1 - exp(pred) / (1 + exp(pred)), rbinom, n = kmax, size = 1)
    seen <- cbind(1, t(seen))[, 1:k]
  }
  ## or missed if after C with T - C ~ truncated Weibull(shape, 1)
  else {
    loss <- data$t - qweibull(runif(n) * pweibull(data$t, shape), shape)
    seen <- sched <= matrix(loss, nrow = n, ncol = ncol(sched))
  }
  if (!length(dim(seen))) seen <- matrix(seen, nrow = n, byrow = TRUE)
  tobs <- apply(sched * seen * (sched < cens), 1, function(x) x[x > 0])
  if (!is.list(tobs))
    tobs <- do.call(c, apply(t(tobs), 1, list))
  g <- function(x, y, vec) {
    vec <- c(y, vec, Inf)
    ind <- findInterval(x, vec, rightmost.closed = TRUE)
    vec[c(ind, ind + 1)]
  }
  int <- t(mapply(g, x = data$time, y = data$trun, vec = tobs))
  int[is.infinite(int[, 2]), 2] <- NA
  data <- cbind(data, cens = cens, left = int[, 1], right = int[, 2])
  ## imputation to mid- and right-endpoints
  data$mid <- (data$left + data$right)/2
  data$mid[is.na(data$right)] <- data$left[is.na(data$right)]
  data$end <- data$right
  data$end[is.na(data$right)] <- data$left[is.na(data$right)]
  ## right-censor right-endpoint-imputed time if the censoring interval contains
  ## two or more consecutive misses at their preceding negative inspection
  data$last <- t(apply(seen, 1, is.subset, x = c(0, 0))) * sched
  data$last <- apply(with(data, (left < last) * (last < right)) * sched, 1, max)
  data$last[with(data, last == 0 | is.na(last))] <- tau
  data$ttp.time <- data$end
  data$ttp.event <- (1 - is.na(data$right)) * (data$ttp.time <= data$last)
  data$ttp.time <- pmin(data$ttp.time, data$last)
  ## right-censor exact times
  data$event <- data$event * (data$time <= pmin(cens, tau))
  data$time <- pmin(data$time, cens, tau)
  ncens <- sum(data$event == 0)
  ## left-censored
  nleft <- with(data, sum(left == trun))
  ## right-censored
  nright <- sum(is.na(data$right))
  list(data = data, censor = c(ncens, nleft, n - nleft - nright, nright),
       k = k, kmax = kmax, tau = tau, sd = sd, depend = depend, beta = beta,
       cens.shape = shape, schemes = c("ic", "mid", "end", "ttp", "rc"),
       formula = paste(paste("Surv(",
         c("left, right, type = 'interval2'", "mid, !is.na(right)",
           "end, !is.na(right)", "ttp.time, ttp.event", "time, event"), ")",
         sep = ""), model, sep = " ~ "))
}
