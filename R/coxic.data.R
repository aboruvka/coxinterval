### format data for C fitting routine
coxic.data <- function(id, time1, time2, from, to, status, z, states) {
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
  ## NA action permits missing 'time1' when 'time1' = 'time2'
  time1[is.na(time1)] <- time2[is.na(time1)]
  ## largest and smallest observation times
  u <- as.vector(by(time1, id, min))
  v <- as.vector(by(time2, id, max))
  ## T observed?
  absorb <- is.element(uid, id[to == states[3] & status == 1])
  ## contribution via 0 -> 1 (1), 0 -> 2 (2), both (0)?
  contrib <- rep(2, n)
  contrib[uid %in% id[from %in% states[2]]] <- 1
  contrib[uid %in% id[is.na(from)]] <- 0
  ## (possible) censoring intervals (L, R] with L = R if T01 observed exactly
  left <- time2[to == states[2]]
  left[!absorb & contrib == 2] <- v[!absorb & contrib == 2]
  right <- rep(Inf, n)
  right[contrib == 0] <- time2[is.na(from)]
  right[contrib == 1] <- time1[from %in% states[2]]
  right[absorb & contrib == 0] <- v[absorb & contrib == 0] - .Machine$double.eps
  ## maximal intersections containing 0 -> 1 support
  maxint <- cbind(left, right)[contrib != 0, ]
  t01 <- maximalint(maxint)$int[, 2]
  t02 <- v[absorb & contrib == 2]
  t12 <- v[absorb & contrib == 1]
  list(supp = list(t01 = t01, t02 = sort(unique(t02)), t12 = sort(unique(t12))),
       left = left, right = right, u = u, v = v, contrib = contrib,
       absorb = absorb, z = z)
}
