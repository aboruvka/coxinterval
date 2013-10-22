### wrapper for survival's Surv
Surv <- function(time, time2, event,
                 type = c("right", "left", "interval", "counting", "interval2",
                   'interval3'), origin = 0) {
  nargs <- 3 - missing(time) - missing(time2) - missing(event)
  if (missing(type)) type <- c(rep("right", 2), "counting")[nargs]
  if (type != "interval3")
    survival::Surv(time, time2, event, type, origin)
  else {
    s <- survival::Surv(time, time2, 1*(event %in% c(1, 3)), type, origin)
    s[, 3][event %in% 3] <- 3
    attr(s, "type") <- "interval3"
    s
  }
}

print.Surv <- function(x, quote = FALSE, ...) {
  if (attr(x, "type") == "interval3") attr(x, "type") <- "counting"
  invisible(print(as.character(x), quote = quote, ...))
}
