coxdual.strata <- function(from, to)
{
  if (missing(to))
    stop("The 'strata' term needs two arguments for the 'from' and 'to' states.")
  x <- cbind(table(from, to), sort(unique(from)), sort(unique(to)))
  state1 <- x[apply(x[, 1:2] == 0, 1, any), 3]
  state0 <- x[x[, 3] != state1, 3]
  state2 <- x[x[, 4] != state1, 4]
  x <- cbind(from, to)
  states <- c(state0, state1, state2)
  cl <- match.call()
  types <- cl[c(1, match(c("from", "to"), names(cl), nomatch = 0))]
  types[[1]] <- as.name("strata")
  types <- levels(eval(types, parent.frame()))
  if (length(states) != 3 | length(types) != 3)
    stop("Invalid state transitions in the model 'strata' term.")
  attr(x, "states") <- states
  x
}
