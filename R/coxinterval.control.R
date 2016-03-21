### set parameters controlling the model fit
coxinterval.control <-
  function(eps = 1e-7, iter.max = 5000, coef.typ = 1, coef.max = 10,
           return.data = FALSE, eps.norm = c("max", "grad"), armijo = 1/3,
           var.coef = TRUE, trace = FALSE, thread.max = 1, sieve = TRUE,
           sieve.const = 1, sieve.rate = 1/3, risk.min = 1)
{
  eps.norm <- match.arg(eps.norm)
  if (eps <= .Machine$double.eps)
    stop("Invalid epsilon. Choose a small value > ", .Machine$double.eps, ".")
  if (!is.element(eps.norm, c("max", "grad")))
    stop(paste("Unknown stopping rule norm", eps.norm))
  if (iter.max < 1)
    stop("Invalid maximum iterations. Choose a large positive integer.")
  if (var.coef & coef.typ < eps)
    stop("Invalid coefficient magnitude. Choose a positive value.")
  if (var.coef & coef.max <= coef.typ)
    stop("Invalid maximum coefficient size. Choose a value > ", coef.typ, ".")
  if (armijo < eps | armijo >= 1/2)
    stop("Invalid scale for Armijo's rule. Choose a value in (0, 1/2).")
  if (any(sieve.const < eps))
    stop("Invalid sieve constant. Choose a positive value.")
  if (sieve & length(sieve.const != 3))
    sieve.const <- rep(sieve.const[1], 3)
  if (sieve & (sieve.rate <= 1/8 | sieve.rate >= 1/2))
    stop("Invalid sieve rate. Choose a value in (1/8, 1/2).")
  if (risk.min < 1 | risk.min > 2)
    stop("Minimum risk set should be 1 or 2.")
  list(eps = eps, iter.max = iter.max, return.data = return.data,
       coef.typ = coef.typ, coef.max = coef.max, eps.norm = eps.norm,
       armijo = armijo, var.coef = var.coef, trace = trace,
       thread.max = thread.max, sieve = sieve, sieve.const = sieve.const,
       sieve.rate = sieve.rate, risk.min = risk.min)
}
