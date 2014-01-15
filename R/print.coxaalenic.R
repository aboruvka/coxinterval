print.coxaalenic <- function(x, digits = max(options()$digits - 4, 3)) {
  cat("Call:\n")
  dput(x$call)
  saved.digits <- options(digits = digits)
  on.exit(options(saved.digits))
  f <- function(x, formula = TRUE) {
    if (formula) {
      cat("\n")
      cat("Formula:\n")
      dput(x$call$formula)
    }
    if (is.null(x$coef)) {
      cat("  Estimation failed.\n")
    }
    else {
      est <- x$coef
      se <- sqrt(diag(x$var))
      temp <- cbind(est, se, est/se,
                    signif(1 - pchisq((est/se)^2, 1), digits - 1), exp(est),
                    signif(exp(est - qnorm(0.975) * se), digits),
                    signif(exp(est + qnorm(0.975) * se), digits))
      dimnames(temp) <-
        list(names(est),
             c("coef", "se(coef)", "z", "p", "exp(coef)", "2.5%", "97.5%"))
      cat("\n")
      prmatrix(temp)
    }
  }
  f(x, formula = FALSE)
  cat("\n")
  cat("Based on n =", x$n)
  if (length(x$na.action)) cat(" (", naprint(x$na.action), ")\n", sep="")
  else cat("\n")
  options(digits = ceiling(log10(x$n)) + digits)
  cat("Initial log-likelihood:", x$loglik[1], "\n")
  cat("Log-likelihood after", x$iter, "iterations:", x$loglik[x$iter], "\n")
  options(digits = digits)
  temp <- t(x$censor.rate)
  dimnames(temp) <- list("Censoring rate", c("Left", "Interval", "Right"))
  cat("\n")
  prmatrix(temp)
  if (length(x$rcfit)) {
    cat("\n")
    cat("Estimation from imputed data via timereg's cox.aalen function\n")
    lapply(x$rcfit, f)
  }
}
