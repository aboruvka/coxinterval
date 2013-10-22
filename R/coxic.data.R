### format data for C fitting routine
coxic.data <- function(id, time1, time2, from, to, status, z, states, censor) {
  p <- max(1, ncol(z))
  z <- data.frame(id, from, to, status, z)
  names(z) <- c("id", "from", "to", "status", paste("z", 1:p, sep = ""))
  ## type-specific covariates (nb: ? -> 2 presumed to hold values for 1 -> 2)
  z <- merge(merge(subset(z, from == states[1] & to == states[2]),
                   subset(z, from == states[1] & to == states[3]), by = "id"),
             subset(z, is.element(from, c(states[2], NA)) & to == states[3]),
             by = "id", all = TRUE)
  z <- z[, substr(names(z), 1, 1) == "z"]
  z[is.na(z)] <- 0
  names(z) <- paste(paste("z", 1:p, ".", sep= ""),
                    rep(c("01", "02", "12"), each = p), sep = "")
  rownames(z) <- NULL
  uid <- unique(id)
  n <- length(uid)
  ## T observed?
  absorb <- is.element(uid, id[to == states[3] & status == 1])
  if (censor == "counting") {
    ## contribution via 0 -> 1 (1), 0 -> 2 (2), both (0)?
    contrib <- rep(2, n)
    contrib[is.element(uid, id[to == states[2] & status == 1])] <- 1
    contrib[is.element(uid, id[is.na(from)])] <- 0
    ## (possible) censoring intervals (L, R] with L = R if T01 observed exactly
    left <- right <- rep(NA, n)
    left[contrib == 1] <- time2[to == states[2] & status == 1]
    right[contrib == 1] <- time2[to == states[2] & status == 1]
    left[contrib == 0] <- time1[is.na(from)]
    right[contrib == 0] <- time2[is.na(from)]
    ind <- absorb & contrib == 0
    if (any(ind)) right[ind] <- right[ind] - .Machine$double.eps
    u <- as.vector(by(time1, id, min))
    ## V = min(T, D)
    v <- as.vector(by(time2, id, max))
    ## observed times
    t01 <- sort(unique(right[contrib == 1]))
  }
  else {
    time2[status == 0] <- Inf
    ## consider only finite times
    ind <- is.finite(time1)
    id <- id[ind]
    time1 <- time1[ind]
    time2 <- time2[ind]
    status <- status[ind]
    from <- from[ind]
    to <- to[ind]
    uid <- unique(id)
    n <- length(uid)
    ## left and right endpoints of (possible) censoring intervals (L, R]
    left <- right <- rep(NA, n)
    ind <- to == states[2]
    left[uid %in% id[ind]] <- time1[ind]
    right[uid %in% id[ind]] <- time2[ind]
    u <- as.vector(by(time1, id, min, na.rm = TRUE))
    v <- as.vector(by(time1, id, max, na.rm = TRUE))
    ind <- left == v
    right[ind] <- Inf
    ## contribution via 0 -> 1 (1), 0 -> 2 (2), both (0)?
    contrib <- rep(0, n)
    contrib[!(uid %in% id[from == states[1] & to == states[3]])] <- 1
    contrib[!(uid %in% id[to == states[2]]) | left == v] <- 2
    ## maximal intersections containing 0 -> 1 support
    maxint <- cbind(left, right, 3)[contrib == 1, ]
    maxint[is.na(maxint[, 2]), 3] <- 0
    maxint <- maximalint(maxint)$int
    t01 <- maxint[, 2] 
  }
  t02 <- v[absorb & contrib == 2]
  t12 <- v[absorb & contrib == 1]
  list(supp = list(t01 = t01, t02 = sort(unique(t02)), t12 = sort(unique(t12))),
       left = left, right = right, u = u, v = v, contrib = contrib,
       absorb = absorb, z = z)
}
