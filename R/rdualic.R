### generate interval-censored data from a Markov illness-death process
rdualic <- function(data, alpha = c(1/4, 1, 1, 1), coef = c(rep(-log(2), 2), 0),
                    type.fun = rdualic.type, ...) {
  n <- nrow(data)
  data$time <- data$s
  data$event <- 1
  ic <- rsched(data, ...)
  ic$data <- subset(ic$data, select = -c(time, event, ttp.time, ttp.event))
  ic$data$right[is.na(ic$data$right)] <- Inf
  ## V = min(T, C)
  ic$data$v <- with(ic$data, pmin(t, cens))
  ## last obs yields inspection with prob p, logit(p) = t(1, cv) %*% alpha
  alpha <- log(alpha)
  pred <- alpha[1] + as.matrix(subset(data, select = z1:z3)) %*% alpha[-1]
  ic$data$insp <- sapply(exp(pred) / (1 + exp(pred)), rbinom, n = 1, size = 1)
  ic$data$insp
  ## update censoring interval
  ## V < R: (L, R] => (L, V-] or (L, V]
  ind <- with(ic$data, v < right)
  ic$data$right[ind] <- with(ic$data[ind, ], v - (v == t) * .Machine$double.eps)
  ## ... unless status observed at V = S = T
  ind <- with(ic$data, insp & v == s & s == t)
  ic$data$left[ind] <- ic$data$right[ind] <- Inf
  ## ... or status observed at V < S
  ind <- with(ic$data, insp & v < s)
  ic$data$left[ind] <- ic$data$v[ind]
  ic$data$right[ind] <- Inf
  ## calculate mid- and endpoints
  ic$data$mid <- ic$data$end <- ic$data$v
  ind <- with(ic$data, right < v - (v == t) * .Machine$double.eps)
  ic$data$end[ind] <- ic$data$right[ind]
  ind <- ind | with(ic$data, insp & s <= v & s < t)
  ic$data$mid[ind] <- with(ic$data[ind, ], right + left) / 2
  ## observed survival data following the 'counting' censoring type:
  ## (start, stop] = known at-risk interval for from -> to transition
  ## status = 0 no from -> to transition on (start, stop]
  ##          1 from -> to transition at stop
  obs <-
    subset(ic$data,
           select = c(id, trun, left, right, mid, end, s, t, v, insp, last))
  ## 0 -> 1: (U, L, 0) or (U, V, 0)
  obs$start <- with(obs, pmin(trun, left))
  obs$stop <- with(obs, pmin(left, v))
  obs$from <- obs$status <- 0
  obs$to <- 1  
  ## 0 -> 2: (U, L) or (U, V)
  temp <- obs
  temp$status <- 1 * with(temp, stop == t)
  temp$to <- 2
  obs <- rbind(obs, temp)
  ## ? or 1 -> 2: (R, V)
  temp <- subset(obs, to == 1 & right <= v)
  if (nrow(temp)) {
    temp$start <- temp$right
    temp$stop <- temp$v
    temp$status <- 1 * with(temp, v == t)
    temp$from <- NA
    temp$from[with(temp, insp | right < v - .Machine$double.eps)] <- 1
    temp$to <- 2
  }
  obs <- rbind(obs, temp)
  ## midpoint-imputed progression in three-state model
  mid <- obs
  ## 0 -> 1  mid < V, {S = T} not observed: (U, mid, 1)
  ##         else: (U, V, 0)
  ## 0 -> 2  mid = V = T: (U, T, 1)
  ##         else (U, mid, 0)
  mid$mid.time1 <- mid$trun
  mid$mid.time2 <- with(mid, pmin(mid, v))
  mid$stratum <- with(mid, from * 10 + to)
  mid$stratum[is.na(mid$stratum)] <- 12
  mid$mid.event <- 0
  mid$mid.event[with(mid, stratum == 1 & mid < v)] <- 1
  mid$mid.event[with(mid, stratum == 2 & mid == t & t == v)] <- 1
  ## 1 -> 2  (mid, V, 1(V = T))
  ind <- mid$stratum == 12
  if (any(ind)) {
    mid$mid.time1[ind] <- mid$mid[ind]
    mid$mid.time2[ind] <- mid$v[ind]
    mid$mid.event[ind] <- with(mid[ind, ], t == v)
  }
  ## progression-free survival, all available assessments
  pfs <- subset(mid, to == 1)
  pfs$time <- pfs$end
  pfs$event <- 1 * with(pfs, end < v | end == t)
  ## progression-free survival, censoring before two or more missed visits
  pfs2 <- pfs
  pfs2$time <- with(pfs2, pmin(last, time))
  pfs2$event <- 1 * with(pfs2, event == 1 & time < last)
  pfs <- subset(pfs, select = c(id, time, event, from, to))
  names(pfs)[2:3] <- paste("pfs", names(pfs)[2:3], sep = ".")
  pfs2 <- subset(pfs2, select = c(id, time, event, from, to))
  names(pfs2)[2:3] <- paste("pfs2", names(pfs2)[2:3], sep = ".")
  ## latent right-censored data
  rc <- mid
  rc$time1 <- rc$trun
  rc$time2 <- with(rc, pmin(s, v))
  rc$event <- 0
  rc$event[with(rc, stratum == 1 & s < t & s <= v)] <- 1
  rc$event[with(rc, stratum == 2 & s == t & t == v)] <- 1
  ind <- rc$stratum == 12
  if (any(ind)) {
    rc$time1[ind] <- rc$s[ind]
    rc$time2[ind] <- rc$v[ind]
    rc$event[ind] <- with(rc[ind, ], t == v)
  }
  obs <- subset(obs, select = c(id, start, stop, from, to, status))
  mid <-
    subset(mid,
           select = c(id, mid.time1, mid.time2, mid.event, from, to, stratum))
  rc <- subset(rc, select = c(id, time1, time2, from, to, event))
  by.name <- c("id", "from", "to")
  ic$data <-
    merge(ic$data,
          merge(merge(obs, merge(mid, pfs, by = by.name, all = TRUE),
                      by = by.name, all = TRUE),
                merge(pfs2, rc, by = by.name, all = TRUE),
                by = by.name, all = TRUE), by = "id", all = TRUE)
  ic$data <- ic$data[with(ic$data, order(id, from, to, start, stop)), ]
  uid <- unique(ic$data$id)
  ## left-truncated
  trun <- uid %in% with(ic$data, id[trun > 0])
  ## event-free at C: S > C
  free <- uid %in% with(ic$data, id[s > cens])
  ## surviving at C: T > C
  surv <- uid %in% with(ic$data, id[t > cens])
  ## S < C, T
  prog <- uid %in% with(ic$data, id[s <= cens & s < t])
  ## progression status known at min(T, tau)
  status <- uid %in%
    with(ic$data, id[insp == 1 | (right < v - (v == t) * .Machine$double.eps)])
  ## rates
  obs.rate <-
    list(overall = c(trun = sum(trun), free = sum(free), surv = sum(surv),
           prog = sum(prog), status = sum(status)) / n,
         free = c(trun = sum(trun & free),
           status = sum(status & free)) / sum(free),
         prog = c(trun = sum(trun & prog),
           status = sum(status & prog)) / sum(prog),
         noprog = c(trun = sum(trun & prog),
           status = sum(status & !free & !prog)) / sum(!free & !prog))
  ## type-specific covariates
  type <- type.fun(ic$data)
  type$lab <-
    sapply(type$lab, function(x) if (x == "") "" else paste(".", x, sep = ""))
  z <- subset(ic$data, select = z1:z3)
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
  ic$data <- cbind(ic$data, type.z[, !is.element(colnames(type.z), z.name)])
  rownames(data) <- NULL
  coef.name <- colnames(type.z)
  ## models
  model <-
    paste(c(coef.name, "cluster(id)", "strata(stratum)"), collapse = " + ")
  model.ic <-
    paste(c(coef.name, "cluster(id)", "trans(from, to)"), collapse = " + ")
  model.pfs <- paste(type.surv[!duplicated(type.surv)], collapse = " + ")
  ic$obs.rate <- obs.rate
  ic$alpha <- alpha
  ic$coef.name <- coef.name
  ic$coef <- coef
  ic$type.surv <- (1:ncol(type.z))[!duplicated(type.surv)]
  ic$schemes <- c("ic", "mid", "pfs", "pfs2", "rc")
  ic$schemes.surv <- 3:4
  ic$formula <-
    c(paste("Surv(start, stop, status, type = 'interval') ~", model.ic),
      paste("Surv(",
            paste(rep("mid", 3), c("start", "stop", "status"),
                  sep = ".", collapse = ", "), ") ~ ", model, sep = ""),
      paste("Surv(trun, ",
            sapply(c("pfs.", "pfs2."), paste, c("time", "event"), sep = "",
                   collapse = ", "), ") ~ ", model.pfs, sep = ""),
      paste("Surv(time1, time2, event) ~", model))
  ic
}

rdualic.type <- function(data)
  list(ind = with(data, list(NULL, NULL, cbind(to == 1, from %in% 0 & to == 2,
         from %in% c(1, NA)))), lab = c("01", "02", "12"))
