### is the (ordered) vector x a subset of set? (extension of is.element)
is.subset <- function(x, set) {
  cand <- 1:(length(set) - length(x) + 1)
  for (i in 1:length(x)) cand <- cand[x[i] == set[cand + i - 1]]
  if (!length(cand)) cand <- 0
  sapply(1:length(set), is.element, set = cand)
}
