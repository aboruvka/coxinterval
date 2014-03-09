print.coxinterval <- function(x, ...)
{
  cat("Call:\n")
  dput(x$call)
  est <- x$coef
  se <- sqrt(diag(x$var))
  mat <- cbind(est, se, est/se, 1 - pchisq((est/se)^2, 1), exp(est))
  dimnames(mat) <- list(names(est), c("coef", "se(coef)", "z", "p", "exp(coef)"))
  cat("\n")
  prmatrix(mat)
  cat("\n")
  cat("Initial log-likelihood:", x$loglik[1], "\n")
  cat("Log-likelihood after", x$iter, "iterations:", x$loglik[x$iter], "\n")
  invisible(x)
}
