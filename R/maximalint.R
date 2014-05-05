maximalint <- function(x)
{
  if (is.null(nrow(x))) x <- matrix(x, nrow = 1)
  if (ncol(x) == 2) x[is.na(x[, 2]), 2] <- Inf
  else if (ncol(x) > 2) {
    ## right-censored
    x[x[, 3] == 0, 2] <- Inf
    ## exact
    x[x[, 3] == 1, 2] <- x[x[, 3] == 1, 1]
  }
  ind <- x[, 1] == x[, 2]
  ## break ties
  if (sum(!ind)) {
    l <- unique(x[!ind, 1])
    r <- unique(x[!ind, 2])
    if (max(l, r[r < Inf]) <= 2) delta <- .Machine$double.eps
    else {
      s <- sort(unique(c(l, r)))
      if (length(s) > 1)
        delta <- min(s[-1] - s[-length(s)]) / 10
      else
        delta <- s / 10
      r[r %in% l] <- r[r %in% l] - delta
    }
  }
  else l <- r <- NULL
  if (sum(ind)) {
    x[ind, 1] <- x[ind, 2] - .Machine$double.eps
    l <- c(l, unique(x[ind, 1]))
    r <- c(r, unique(x[ind, 2]))
  }
  s <- rbind(cbind(l, 0), cbind(r, 1))
  s <- s[order(s[, 1]), ]
  s <- cbind(s, rbind(s[-1, ], NA))
  s <- s[s[, 2] == 0 & s[, 4] == 1, c(1, 3)]
  if (is.null(nrow(s))) s <- matrix(s, nrow = 1)
  ## indicator matrix
  if (nrow(s) < 2)
    i <- matrix(1)
  else
    i <-
      t(apply(x[, 1:2], 1, function(x) 1 * (s[, 1] >= x[1] & s[, 2] <= x[2])))
  colnames(s) <- c("s", "t")
  list(int = s, ind = i)
}
