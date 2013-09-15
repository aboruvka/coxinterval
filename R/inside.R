### indicate which entries of a vector lie in the interval (start, stop]
inside <- function(v, start, stop, closed = TRUE) {
  if (closed) z <- sapply(v, function(x) x > start & x <= stop)
  else z <- sapply(v, function(x) x > start & x < stop)
  if (length(start) > 1) t(z)
  else t(t(z))
}
