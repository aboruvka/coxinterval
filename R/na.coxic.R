### custom NA action adapted from na.omit.data.frame
na.coxic <- function(object, ...) UseMethod("na.coxic")
na.coxic.default <- function(object, ...) {
  n <- length(object)
  omit <- FALSE
  vars <- seq_len(n)
  for(j in vars) {
    x <- object[[j]]
    if (!is.atomic(x)) next
    # save states attribute for 'trans' term
    # allow missing values for ? -> 2 transitions
    if (!is.null(states <- attr(x, "states"))) {
      types <- attr(x, "types")
      x[is.na(x[, 1]) & x[, 2] %in% states[3], 1] <- -1
    }
    x <- is.na(x)
    d <- dim(x)
    if (is.null(d) || length(d) != 2L)
      omit <- omit | x
    else
      for(ii in 1L:d[2L])
        omit <- omit | x[, ii]
  }
  xx <- object[!omit, , drop = FALSE]
  if (any(omit > 0L)) {
    temp <- seq(omit)[omit]
    names(temp) <- attr(object, "row.names")[omit]
    attr(temp, "class") <- "exclude"
    attr(xx, "na.action") <- temp
  }
  # save states attribute for 'trans' term
  attr(xx, "states") <- states
  attr(xx, "types") <- types
  xx
}
