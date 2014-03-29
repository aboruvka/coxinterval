coxic <- function(formula, data = parent.frame(), subset, init = NULL,
                  formula.coxph = NULL, init.coxph = FALSE, control, ...)
{
  ## extract model frame and perform input validation
  cl <- match.call(expand.dots = FALSE)
  datargs <- c("formula", "data", "subset")
  mf <- cl[c(1, match(datargs, names(cl), nomatch = 0))]
  mf[[1]] <- as.name("model.frame")
  specials <- c("trans", "cluster", "strata", "tt")
  ftrm <- if (missing(data)) terms(formula, specials)
          else terms(formula, data = data, specials)
  if (with(attr(ftrm, "specials"), length(c(strata, tt))))
    stop("The 'strata' and 'tt' terms not supported")
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
  suppressWarnings(mf <- eval(mf, parent.frame()))
  mt <- attr(mf, "terms")
  mm <- model.matrix(mt, mf)
  ## find covariates model matrix
  asgn <- frame.assign(mf, mt, mm)
  jcov <- subset.data.frame(asgn, frame %in% icov)$matrix
  if (!inherits(mf[, irsp], "Surv")) stop("Response is not a 'Surv' object")
  if (attr(mf[, irsp], "type") != "counting")
    stop("Response is not a 'counting'-type 'Surv' object")
  ## in case NA action didn't apply trans attributes
  if (!is.null(attr(mf[, itrn], "states"))) {
    attr(mf, "states") <- attr(mf[, itrn], "states")
    attr(mf, "types") <- attr(mf[, itrn], "types")
  }
  states <- attr(mf, "states")
  if (length(states) != 3 | length(attr(mf, "types")) != 3)
    stop("Invalid state transitions in the model 'trans' term")
  ## sort data
  ord <- order(mf[, icls], mf[, itrn][, 1], mf[, itrn][, 2])
  mf <- mf[ord, ]
  mm <- mm[ord, ]
  ## reworked data
  d <- coxic.data(mf[, icls], mf[, irsp][, 1], mf[, irsp][, 2], mf[, itrn][, 1],
                  mf[, itrn][, 2], mf[, irsp][, 3], mm[, jcov], states)
  ## strictly dual right censoring?
  if (with(d, all(left[contrib == 1] == right[contrib == 1]))) censor <- "right"
  else censor <- "interval"
  ## fit right-censored data alternatives with survival's coxph
  if (censor == "right") {
    formula.coxph <-
      if (is.null(formula.coxph)) list(cl$formula)
      else c(cl$formula, formula.coxph)
  }
  fit.coxph <- list(NULL)
  if (!is.null(formula.coxph))
    for (i in 1:length(formula.coxph)) {
      fit.coxph[[i]] <- cl
      fit.coxph[[i]]$formula <-
        update.formula(fit.coxph[[i]]$formula, formula.coxph[[i]])
      temp <- paste(gsub("trans\\(", "strata\\(",
                         deparse(fit.coxph[[i]]$formula[[3]])), collapse = "")
      fit.coxph[[i]]$formula <-
        update.formula(fit.coxph[[i]]$formula, as.formula(paste("~", temp)))
      temp <- list(formula = fit.coxph[[i]]$formula, data = data,
                   na.action = "na.omit")
      if (!missing(subset)) temp <- c(temp, subset)
      invisible(capture.output(fit.coxph[[i]] <- try(do.call("coxph", temp))))
      if (inherits(fit.coxph[[i]], "try-error"))
        fit.coxph[[i]] <-
          list(call = temp, coef = NULL, var = NULL, bhaz = NULL, loglik = NULL,
               m = NULL, na.action = NULL)
      else {
        fit.coxph[[i]]$basehaz <- basehaz(fit.coxph[[i]], centered = FALSE)
        fit.coxph[[i]]$m <- fit.coxph[[i]]$n
        fit.coxph[[i]]$n <- length(unique(mf[-fit.coxph[[i]]$na.action, icls]))
        if (censor == "right" & i == 1) {
          rownames(fit.coxph[[i]]$var) <-
            colnames(fit.coxph[[i]]$var) <- colnames(mm)[jcov]
          names(fit.coxph[[i]]$basehaz) <- c("hazard", "time", "trans")
        }
        fit.coxph[[i]]$call$data <- cl$data
      }
    }
  else fit.coxph <- NULL
  ## set parameters controlling model fit
  control <- if (missing(control)) coxic.control(...)
             else do.call(coxic.control, control)
  ncov <- length(jcov)
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
  init.coxph <- init.coxph & !is.null(fit.coxph[[1]]) & is.null(init)
  if (is.null(init)) {
    init <- list()
    init$coef <- rep(0, ncov)
    init$basehaz <- tvec / max(d$v)
    bhaz <- NULL
  }
  else {
    names(init) <- c("coef", "bhaz")
    bhaz <- init$basehaz
  }
  if (init.coxph) {
    init$coef <- fit.coxph[[1]]$coef
    bhaz <- fit.coxph[[1]]$bhaz
  }
  if (!is.null(bhaz)) {
    bhaz <- step2jump(bhaz, stratum = 3)
    bhaz <- jump2step(bhaz[bhaz[, 1] > 0, ], stratum = 3)
    init$basehaz <-
      do.call("c", mapply(linapprox, split(bhaz[, 2:1], bhaz[, 3]), part,
                          SIMPLIFY = FALSE))
  }
  init$basehaz <- cbind(init$basehaz, tvec, rep(c(1, 2, 12), times = npart))
  rownames(init$basehaz) <- NULL
  fit <- .C("coxic",
            coef = as.double(init$coef),
            bhaz = as.double(lin2const(init$basehaz, stratum = 3)[, 1]),
            as.integer(ncov),
            as.integer(npart),
            as.double(tvec),
            as.double(svec),
            as.integer(length(svec)),
            as.double(as.matrix(d$z)),
            as.integer(n),
            as.double(d$left),
            as.double(d$right),
            as.double(d$u),
            as.double(d$v),
            as.integer(d$contrib),
            as.integer(d$absorb),
            var = as.double(rep(0, ncov^2)),
            loglik = as.double(rep(0, control$iter.max + 1)),
            as.double(control$eps),
            as.integer(control$iter.max),
            as.double(control$coef.typ),
            as.double(control$coef.max),
            iter = as.integer(0),
            gradnorm = as.double(0),
            maxnorm = as.double(0),
            cputime = as.double(0),
            flag = as.integer(0),
            NAOK = TRUE)
  if (fit$flag == 1)
    stop("Parameter estimation failed; coefficient Hessian not invertible.")
  if (with(fit, any(is.na(coef), is.nan(coef), is.na(bhaz), is.nan(bhaz))))
    stop("Parameter estimation failed.")
  if (fit$flag == 2)
    stop("Variance estimation failed; profile information not invertible.")
  if (with(fit, any(is.na(diag(var)), is.nan(diag(var)))))
    stop("Variance estimation failed.")
  if (with(fit, iter == control$iter.max & maxnorm > control$eps))
    warning("Maximum iterations reached before convergence.")
  names(fit$coef) <- colnames(mm)[jcov]
  var <- matrix(fit$var, ncov)
  rownames(var) <- colnames(var) <- colnames(mm)[jcov]
  init$basehaz <- data.frame(init$basehaz)
  init$init.coxph <- init.coxph
  bhaz <-
    data.frame(const2lin(cbind(fit$bhaz, init$basehaz[, -1]), stratum = 3))
  names(bhaz) <- names(init$basehaz) <- c("hazard", "time", "trans")
  bhaz$trans <- as.factor(bhaz$trans)
  levels(bhaz$trans) <- attr(mf, "types")
  censor.rate <- with(d, c(sum(contrib != 0 & absorb), sum(contrib == 0)))
  censor.rate <- matrix(c(censor.rate[1], n - censor.rate[1] - censor.rate[2],
                          censor.rate[2]) / n, nrow = 1)
  dimnames(censor.rate) <-
    list("Observation rate", c("(S, T)", "1(S < T)", "Neither"))
  fit <- list(call = cl, censor = censor, n = n, m = nrow(mf), p = ncov,
              coef = fit$coef, var = var, basehaz = bhaz, init = init,
              loglik = n * fit$loglik[1:(fit$iter + 1)], iter = fit$iter,
              gradnorm = fit$gradnorm, maxnorm = fit$maxnorm,
              cputime = fit$cputime, fit.coxph = fit.coxph,
              na.action = attr(mf, "na.action"), censor.rate = censor.rate,
              control = control)
  class(fit) <- c("coxic", "coxinterval")
  fit
}
