### generate doubly right-censored data from a Markov illness-death process
rdualrc <- function(data, beta = c(1/2, 1, 1, 1), tau = 2, rate = 1,
                    depend = FALSE, coef = c(-log(2), -log(2), 0),
                    type.fun = rdualrc.type) {
  n <- nrow(data)
  z <- as.matrix(subset(data, select = z1:z3))
  beta <- log(beta)
  pred <- beta[1] + z %*% beta[-1]
  ## doubly censored with prob p, logit(p) = t(1, z) %*% beta
  cens <- sapply(exp(pred) / (1 + exp(pred)), rbinom, n = 1, size = 1)
  ## C exponential with mean rate or T - C truncated exponential
  if (!depend) c <- qexp(pexp(data$trun, rate)
                         + runif(n) * (1 - pexp(data$trun, rate)), rate)
  else c <- data$t - qexp(pexp(data$trun, rate)
                          + runif(n) * (pexp(data$t, rate)
                                        - pexp(data$trun, rate)), rate)
  c[cens == 0] <- tau
  c <- pmin(c, tau)
  ## D administrative censoring
  d <- rep(tau, n)
  data <- cbind(data, c = c, d = d)
  ## 0 -> 2 observed
  add <- subset(data, s == t & s < c)
  if (nrow(add) > 0)
    new <- rbind(with(add, cbind(id, trun, s, 0, 1, 0)),
                 with(add, cbind(id, trun, s, 0, 2, 1)))
  else new <- NULL
  ## 0 -> 1 -> 2 observed
  add <- subset(data, s < t & s < c & t < d)
  if (nrow(add) > 0)
    new <- rbind(new,
                 with(add, cbind(id, trun, s, 0, 1, 1)),
                 with(add, cbind(id, trun, s, 0, 2, 0)),
                 with(add, cbind(id, s, t, 1, 2, 1)))
  ## 0 -> 1 observed, right-censored in 1
  add <- subset(data, s < c & d < t)
  nright <- nrow(add)
  if (nrow(add) > 0)
    new <- rbind(new,
                 with(add, cbind(id, trun, s, 0, 1, 1)),
                 with(add, cbind(id, trun, s, 0, 2, 0)),
                 with(add, cbind(id, s, d, 1, 2, 0)))
  ## right-censored in 0
  add <- subset(data, c == d & d < s)
  if (nrow(add) > 0)
    new <- rbind(new,
                 with(add, cbind(id, trun, c, 0, 1, 0)),
                 with(add, cbind(id, trun, c, 0, 2, 0)))
  ## T observed, unknown progression event
  add <- subset(data, c < s & t < d)
  nstatus <- nrow(add)
  if (nrow(add) > 0)
    new <- rbind(new,
                 with(add, cbind(id, trun, c,  0, 1, 0)),
                 with(add, cbind(id, trun, c,  0, 2, 0)),
                 with(add, cbind(id, c, t, NA, 2, 1)))
  ## right-censored in 0 or 1
  add <- subset(data, c < s & c < d & d < t)
  if (nrow(add) > 0)
    new <- rbind(new,
                 with(add, cbind(id, trun, c,  0, 1, 0)),
                 with(add, cbind(id, trun, c,  0, 2, 0)),
                 with(add, cbind(id, c, d, NA, 2, 0)))
  ## observed counting process type survival data
  new <- data.frame(new)
  names(new) <- c("id", "start", "stop", "from", "to", "status")
  new <- new[with(new, order(id, start, stop, from, to)), ]
  data <- cbind(new, data[rep(data$id, table(new$id)), -1])
  ## PFS right-censored at C
  data$pfs1.event <- data$pfs1.time <- NA
  data$pfs1.time[data$to == 1] <- with(data, stop[to == 1])
  data$pfs1.event[data$to == 1] <- with(data, status[to == 1])
  ind <- data$id %in% subset(data, from == 0 & to == 2 & status)$id
  data$pfs1.event[ind] <- 1
  ## PFS = (1 - 1(S > C & T < D))*min(S,C) + 1(S > C & T < D)*min(T,D)
  temp <- with(data, (1 - (s < c))*(t < d))
  data$pfs2.time <- with(data, (1 - temp)*pmin(c, s) + temp*pmin(t, d))
  data$pfs2.event <- with(data, pmax(s < c, t < d))
  ind <- data$to == 1
  data$pfs2.event[!ind] <- data$pfs2.time[!ind] <- NA
  ## latent singly right-censored data
  data$time1 <- with(data, (start == trun) * start + (start > trun) * pmin(s, d))
  data$time2 <-
    with(data, (start == trun) * pmin(s, d) + (start > trun) * pmin(t, d))
  data$stratum <- with(data, from * 10 + to)
  data$event <- with(data, (from == 0 & to == 1 & s < t) * (time2 == s)
                     + (to == 2) * (time2 == t))
  data$stratum[with(data, is.na(stratum) & time1 < time2)] <- 12
  data$time1[is.na(data$stratum)] <- data$time2[is.na(data$stratum)] <- NA
  uid <- unique(data$id)
  ## left-truncated
  trun <- uid %in% with(data, id[trun > 0])
  ## progressed on (0, inf)
  prog <- uid %in% with(data, id[s < t])
  ## event-free at D: S > D
  free <- uid %in% with(data, id[s > d])
  ## survived to D: T > D
  surv <- uid %in% with(data, id[t > d])
  ## (S, T) exactly observed
  exact <- uid %in% with(data, id[t < c])
  ## singly right-censored
  rcens <- !exact & uid %in% with(data, id[c == d | s < c])
  ## doubly right-censored
  dcens <- !exact & !rcens
  ## rates
  obs.rate <-
    list(overall = c(trun = sum(trun), prog = sum(prog), free = sum(free),
           surv = sum(surv), exact = sum(exact), rcens = sum(rcens),
           dcens = sum(dcens))/n,
         prog = c(trun = sum(trun & prog), free = sum(free & prog),
           surv = sum(surv & prog), exact = sum(exact & prog),
           rcens = sum(rcens & prog), dcens = sum(dcens & prog))/sum(prog),
         noprog = c(trun = sum(trun & !prog), free = sum(free & !prog),
           surv = sum(surv & !prog), exact = sum(exact & !prog),
           rcens = sum(rcens & !prog), dcens = sum(dcens & !prog))/sum(!prog))
  ## type-specific covariates
  type <- type.fun(data)
  type$lab <-
    sapply(type$lab, function(x) if (x == "") "" else paste(".", x, sep = ""))
  z <- subset(data, select = z1:z3)
  z.name <- paste("z", 1:3, sep = "")
  type.surv <- NULL
  f <- function(x) {
    out <- z[, x] * type$ind[[x]]
    if (length(dim(out))) {
      colnames(out) <- paste(z.name[x], type$lab, sep = "")
      type.surv <<- c(type.surv, rep(z.name[x], ncol(out)))
    }
    out
  }
  type.z <- sapply(1:3, f, simplify = FALSE)
  type.z <- do.call("cbind", type.z)
  data <- cbind(data, type.z[, !is.element(colnames(type.z), z.name)])
  rownames(data) <- NULL
  coef.name <- colnames(type.z)
  ## models
  model.dbl <-
    paste(c(coef.name, "cluster(id)", "trans(from, to)"), collapse = " + ")
  model.pfs <- paste(type.surv[!duplicated(type.surv)], collapse = " + ")
  model.rc <-
    paste(c(coef.name, "cluster(id)", "strata(stratum)"), collapse = " + ")
  type.surv <- (1:ncol(type.z))[!duplicated(type.surv)]
  list(data = data, obs.rate = obs.rate, rate = rate, depend = depend,
       beta = beta, tau = tau, coef = coef, coef.name = coef.name,
       type.surv = type.surv, schemes = c("dbl", "stp", "pfs1", "pfs2", "rc"),
       schemes.surv = 3:4,
       formula = c(paste("Surv(start, stop, status) ~", model.dbl),
         paste("Surv(trun, pfs1.time, pfs1.event) ~", model.pfs),
         paste("Surv(trun, pfs2.time, pfs2.event) ~", model.pfs),
         paste("Surv(time1, time2, event) ~", model.rc)))
}

rdualrc.type <- function(data)
  list(ind = with(data, list(NULL, NULL, cbind(to == 1, from %in% 0 & to == 2,
         from %in% c(1, NA)))), lab = c("01", "02", "12"))
