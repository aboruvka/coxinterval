### find maximal intersections from an interval-type survival object
maximalint <- function(x) {
  x[x[, 3] == 0, 2] <- Inf
  l <- unique(x[, 1])
  r <- unique(x[, 2])
  if (max(l, r[r < Inf]) <= 2) delta <- .Machine$double.eps
  else {
    s <- sort(unique(c(l, r)))
    delta <- min(s[-1] - s[-length(s)]) / 10
  }
  # break ties
  r[r %in% l] <- r[r %in% l] - delta
  s <- rbind(cbind(l, 0), cbind(r, 1))
  s <- s[order(s[, 1]), ]
  s <- cbind(s, rbind(s[-1, ], NA))
  s <- s[s[, 2] == 0 & s[, 4] == 1, c(1, 3)]
  # indicator matrix
  i <- t(apply(x[, 1:2], 1, function(x) 1 * (s[, 1] >= x[1] & s[, 2] <= x[2])))
  colnames(s) <- c("l", "r")
  list(int = s, ind = i)
}
