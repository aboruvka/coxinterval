print.summary.coxinterval <-
  function(x, digits = max(3, getOption("digits") - 4), ...)
{
  cat("Call:\n")
  dput(x$call)
  saved.digits <- options(digits = digits)
  on.exit(options(saved.digits))
  f <- function(fit, formula = TRUE) {
    if (formula) {
      cat("\n")
      cat("Formula:\n")
      dput(fit$formula)
    }
    if (is.null(fit$mat)) cat("  Estimation failed\n")
    else {
      fit$mat[, grep("z", colnames(fit$mat))] <-
        signif(fit$mat[, grep("z", colnames(fit$mat))], digits - 1)
      cat("\n")
      prmatrix(fit$mat)
      cat("\n")
      cat("Based on ")
      cat("n =", fit$n)
      if (!is.null(fit$m))
        cat(" subjects contributing", fit$m, "observation times")
      if (length(fit$na.action)) {
        if (!is.null(fit$m)) cat("\n(")
        else cat(" (")
        cat(length(fit$na.action), "deleted due to missingness)\n")
      }
      else cat("\n")
    }
  }
  f(x, formula = FALSE)
  options(digits = ceiling(log10(x$n)) + digits)
  cat("\n")
  cat("Initial log-likelihood:", x$loglik[1], "\n")
  cat("Log-likelihood after", x$iter, "iterations:", x$loglik[x$iter], "\n")
  options(digits = digits)
  cat("\n")
  prmatrix(x$censor.rate)
  if (length(x$rcfit)) {
    cat("\n")
    if (is.null(x$censor))
      cat("Estimation from imputed data via timereg's cox.aalen function\n")
    else if (x$censor == "right")
      cat("Estimation from right-censored data via survival's coxph function\n")
    else
      cat("Estimation from imputed data via survival's coxph function\n")
    lapply(x$rcfit, f)
  }
  invisible()
}