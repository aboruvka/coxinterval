### fit a Cox model to an interval-censored Markov illness-death process
coxic <- function(formula, data = parent.frame(), subset, init = NULL,
                  rcprog = NULL, rcinit = FALSE, control, ...) {
  ## extract model frame and perform input validation
  cl <- match.call(expand.dots = FALSE)
  datargs <- c("formula", "data", "subset")
  mf <- cl[c(1, match(datargs, names(cl), nomatch = 0))]
  mf[[1]] <- as.name("model.frame")
  specials <- c("trans", "cluster")
  ftrm <- if (missing(data)) terms(formula, specials)
          else terms(formula, data = data, specials)
  ## store column indices of terms in model frame
  irsp <- attr(ftrm, "response")
  ityp <- attr(ftrm, "specials")$type
  itrn <- attr(ftrm, "specials")$trans
  icls <- attr(ftrm, "specials")$cluster
  if (length(itrn) != 1) stop("Model requires exactly one 'trans' term")
  if (length(icls) != 1) stop("Model requires exactly one 'cluster' term")
  icov <- (1:(length(attr(ftrm, "variables")) - 1))[-c(irsp, itrn, icls)]
  if (!length(icov)) stop("Model has no covariates")
  mf$formula <- ftrm
  mf$na.action <- as.name("na.coxic")
  mf <- eval(mf, parent.frame())
  if (!inherits(mf[, irsp], "Surv")) stop("Response is not a 'Surv' object")
  censor <- attr(mf[, irsp], "type")
  if (!is.element(censor, c("counting", "interval")))
    stop("Response is not a 'counting'- or 'interval'-type 'Surv' object")
  ## in case NA action didn't apply trans attributes
  if (!is.null(attr(mf[, itrn], "states"))) {
    attr(mf, "states") <- attr(mf[, itrn], "states")
    attr(mf, "types") <- attr(mf[, itrn], "types")
  }
  if (length(attr(mf, "states")) != 3 | length(attr(mf, "types")) != 3)
    stop("Invalid state transitions in the model 'trans' term")
  ## sort data
  mf <- mf[order(mf[, icls], mf[, itrn][, 1], mf[, itrn][, 2]), ]
  ## fit right-censored data alternatives with survival's coxph
  if (censor == "counting")
    rcprog <- if (is.null(rcprog)) list(cl$formula) else c(cl$formula, rcprog)
  else if (any(is.na(mf[, itrn])))
    stop ("NAs in 'trans' term not permitted with 'interval'-type response")
  rcfit <- list(NULL)
  if (!is.null(rcprog))
    for (i in 1:length(rcprog)) {
      rcfit[[i]] <- cl
      rcfit[[i]]$formula <- update.formula(rcfit[[i]]$formula, rcprog[[i]])
      temp <- paste(gsub("trans\\(", "strata\\(",
                         deparse(rcfit[[i]]$formula[[3]])), collapse = "")
      rcfit[[i]]$formula <-
        update.formula(rcfit[[i]]$formula, as.formula(paste("~", temp)))
      temp <-
        list(formula = rcfit[[i]]$formula, data = data, na.action = "na.omit")
      if (!missing(subset)) temp <- c(temp, subset)
      invisible(capture.output(rcfit[[i]] <- try(do.call("coxph", temp))))
      if (inherits(rcfit[[i]], "try-error"))
        rcfit[[i]] <- list(call = temp, coef = NULL, var = NULL, bhaz = NULL,
                           loglik = NULL, m = NULL, na.action = NULL)
      else {
        rcfit[[i]]$bhaz <- basehaz(rcfit[[i]], centered = FALSE)
        rcfit[[i]]$m = rcfit[[i]]$n
        if (censor == "counting" & i == 1) {
          rownames(rcfit[[i]]$var) <- colnames(rcfit[[i]]$var) <- names(mf)[icov]
          names(rcfit[[i]]$bhaz) <- c("hazard", "time", "trans")
        }
        rcfit[[i]]$call$data <- cl$data
      }
    }
  ## set parameters controlling model fit
  control <- if (missing(control)) coxic.control(...)
             else do.call(coxic.control, control)
  ncov <- length(icov)
  ## rework data for fitter
  d <- coxic.data(mf[, icls], mf[, irsp][, 1], mf[, irsp][, 2],
                  mf[, itrn][, 1], mf[, itrn][, 2], mf[, irsp][, 3],
                  mf[, icov], attr(mf, "states"), censor)
  n <- nrow(d$z)
  sieve.size <- with(control, sieve.const * n^sieve.rate)
  nobs <- mapply(function(x, y) max(1, round(length(x)/y)), d$supp, sieve.size)
  ind <- mapply(function(x, y) seq(y, length(x), y), x = d$supp, y = nobs,
                SIMPLIFY = FALSE)
  part <- mapply(function(x, y) c(0, x[y[-c(1, length(y))]], ceiling(max(d$v))),
                 x = d$supp, y = ind, SIMPLIFY = FALSE)
  npart <- sapply(part, length)
  tvec <- do.call("c", part)
  names(tvec) <- NULL
  svec <- sort(unique(tvec))
  ## initial values
  rcinit <- rcinit & !is.null(rcfit[[1]]) & is.null(init)
  if (is.null(init)) {
    init <- list()
    init$coef <- rep(0, ncov)
    init$bhaz <- tvec / max(d$v)
    bhaz <- NULL
  }
  else {
    names(init) <- c("coef", "bhaz")
    bhaz <- init$bhaz
  }
  if (rcinit) {
    init$coef <- rcfit[[1]]$coef
    bhaz <- rcfit[[1]]$bhaz
  }
  if (!is.null(bhaz)) {
    bhaz <- step2jump(bhaz, by = 3)
    bhaz <- jump2step(bhaz[bhaz[, 1] > 0, ], by = 3)
    init$bhaz <- do.call("c", mapply(linapprox, split(bhaz[, 2:1], bhaz[, 3]),
                                     part, SIMPLIFY = FALSE))
  }
  init$bhaz <- cbind(init$bhaz, tvec, rep(c(1, 2, 12), times = npart))
  rownames(init$bhaz) <- NULL
  fit <- .C("coxic",
            coef = as.double(init$coef),
            bhaz = as.double(lin2const(init$bhaz, by = 3)[, 1]),
            as.integer(ncov),
            as.integer(npart),
            as.double(tvec),
            as.double(svec),
            as.integer(length(svec)),
            as.double(as.matrix(d$z)),
            as.integer(n),
            as.double(d$left),
            as.double(d$right),
            as.double(d$v),
            as.integer(d$contrib),
            as.integer(d$absorb),
            var = as.double(rep(0, ncov^2)),
            loglik = as.double(rep(0, control$iter.max + 1)),
            as.double(control$eps),
            as.integer(control$iter.max),
            as.double(control$coef.typ),
            as.double(control$coef.max),
            as.double(control$step.frac),
            iter = as.integer(0),
            fenchel = as.double(0),
            maxnorm = as.double(0),
            cputime = as.double(0),
            flag = as.integer(0),
            NAOK = TRUE)
  if (with(fit, any(flag < 0, is.na(coef), is.nan(coef))))
    stop("Parameter estimation failed.")
  if (with(fit, any(flag > 0, is.na(diag(var)), is.nan(diag(var)))))
    stop("Variance estimation failed.")
  if (fit$iter == control$iter.max)
    warning("Maximum iterations reached before convergence.")
  names(fit$coef) <- names(mf)[icov]
  var <- matrix(fit$var, ncov)
  rownames(var) <- colnames(var) <- names(mf)[icov]
  init$bhaz <- data.frame(init$bhaz)
  init$rcinit <- rcinit
  bhaz <- data.frame(const2lin(cbind(fit$bhaz, init$bhaz[, -1]), by = 3))
  names(bhaz) <- names(init$bhaz) <- c("hazard", "time", "trans")
  bhaz$trans <- as.factor(bhaz$trans)
  levels(bhaz$trans) <- attr(mf, "types")
  censor.rate <- with(d, c(sum(contrib != 0 & absorb), sum(contrib == 0)))
  censor.rate <- c(censor.rate[1], n - censor.rate[1] - censor.rate[2],
                   censor.rate[2]) / n
  names(censor.rate) <-
    if (censor == "counting") c("exact", "single", "double")
    else c("status and survival", "only status", "neither")
  censor <- if (censor == "counting") "right" else "interval"
  fit <- list(call = cl, censor = censor,
              n = n, m = nrow(mf), p = ncov, coef = fit$coef,
              var = var, bhaz = bhaz, init = init,
              loglik = n * fit$loglik[1:(fit$iter + 1)], iter = fit$iter,
              fenchel = fit$fenchel, maxnorm = fit$maxnorm,
              cputime = fit$cputime, rcfit = rcfit,
              na.action = attr(mf, "na.action"), censor.rate = censor.rate,
              control = control)
  class(fit) <- "coxic"
  fit
}
