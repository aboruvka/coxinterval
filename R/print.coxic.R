print.coxic <- function(x, digits = max(options()$digits - 4, 3), ...) {
  cat("Call:\n")
  dput(x$call)
  saved.digits <- options(digits = digits)
  on.exit(options(saved.digits))
  f <- function(x, formula = TRUE, n = FALSE) {
    if (formula) {
      cat("\n")
      cat("Formula:\n")
      dput(x$call$formula)
    }
    if (is.null(x$coef)) {
      cat("  Estimation failed\n")
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
      cat("\n")
      cat("Based on ")
      if (n) cat("n =", x$n, "subjects contributing ")
      cat(x$m, "observation times")
      if (length(x$na.action)) {
        if (n) cat("\n(")
        else cat(" (")
        cat(length(x$na.action), "deleted due to missingness)\n")
      }
      else cat("\n")
    }
  }
  f(x, formula = FALSE, n = TRUE)
  options(digits = ceiling(log10(x$n)) + digits)
  cat("\n")
  cat("Initial log-likelihood:", x$loglik[1], "\n")
  cat("Log-likelihood after", x$iter, "iterations:", x$loglik[x$iter], "\n")
  options(digits = digits)
  temp <- t(x$censor.rate)
  dimnames(temp) <- list("Observation rate", c("(S, T)", "1(S < T)", "Neither"))
  if (x$censor == "interval") dimnames(temp)[[2]][1] <- "T and 1(S < T)"
  cat("\n")
  prmatrix(temp)
  if (length(x$rcfit)) {
    cat("\n")
    if (x$censor == "right")
      cat("Estimation from right-censored data via survival's coxph function\n")
    else
      cat("Estimation from imputed data via survival's coxph function\n")
    lapply(x$rcfit, f)
  }
}
