### set parameters controlling the model fit
coxaalenic.control <-
  function(eps = 1e-7, iter.max = 5000, step.frac = 1/2, step.scale = 1/3,
           coef.typ = 1, coef.max = 10) {
  if (eps <= .Machine$double.eps)
    stop("Invalid epsilon. Choose a small value > ", .Machine$double.eps, ".")
  if (iter.max < 0)
    stop("Invalid maximum iterations. Choose a large positive integer.")
  if (coef.typ < eps)
    stop("Invalid coefficient magnitude. Choose a positive value.")
  if (coef.max <= coef.typ)
    stop("Invalid maximum coefficient size. Choose a value > ", coef.typ, ".")
  if (step.frac < eps | step.frac > 1)
    stop("Invalid step fraction. Choose a value in (0, 1).")
  if (step.scale < eps | step.scale >= 1/2)
    stop("Invalid step scale. Choose a value in (0, 1/2].")
  list(eps = eps, iter.max = iter.max, coef.typ = coef.typ, coef.max = coef.max,
       step.frac = step.frac, step.scale = step.scale)
}
