### fit a Cox-Aalen model to interval-censored failure times
coxaalenic <- function(formula, data = parent.frame(), subset, init = NULL,
                       rcsurv = NULL, rcinit = FALSE, control,
                       close.cplex = FALSE, ...) {
  ## extract model frame and perform input validation
  cl <- match.call(expand.dots = FALSE)
  datargs <- c("formula", "data", "subset")
  mf <- cl[c(1, match(datargs, names(cl), nomatch = 0))]
  mf[[1]] <- as.name("model.frame")
  specials <- c("prop")
  ftrm <- if (missing(data)) terms(formula, specials)
          else terms(formula, data = data, specials)
  ## store column indices of terms in model frame
  irsp <- attr(ftrm, "response")
  iprp <- attr(ftrm, "specials")$prop
  iadd <- (1:(length(attr(ftrm, "variables")) - 1))[-c(irsp, iprp)]
  if (!length(c(iadd, iprp))) stop("Model has no covariates")
  mf$formula <- ftrm
  mf$na.action <- as.name("na.omit")
  mf <- eval(mf, parent.frame())
  if (!inherits(mf[, irsp], "Surv")) stop("Response is not a 'Surv' object")
  mf[, irsp]
  ## Surv converts interval2 to interval
  if (attr(mf[, irsp], "type") != "interval"
      | !(all(mf[, irsp][, 3] %in% c(0, 3))))
    stop("Response is not an 'interval2'-type 'Surv' object")
  ## fit right-censored data alternatives with timereg's cox.aalen
  rcfit <- list()
  if (!is.null(rcsurv)) {
    if (!missing(subset)) warning("Model alternatives not based on subset")
    keep <- 1:nrow(data)
    if (!is.null(omit <- attr(mf, "na.action"))) keep <- keep[-omit]
    for (i in 1:length(rcsurv)) {
      rcfit[[i]] <- cl
      rcfit[[i]]$formula <- update.formula(rcfit[[i]]$formula, rcsurv[[i]])
      ## cox.aalen arguments, constructed to avoid NA-related errors
      temp <-
        list(formula = rcfit[[i]]$formula, data = data[keep, ], robust = 0,
             silent = 1, max.timepoint.sim = nrow(data))
      invisible(capture.output(rcfit[[i]] <- try(do.call("cox.aalen", temp))))
      if (inherits(rcfit[[i]], "try-error"))
        rcfit[[i]] <- list(call = temp, coef = NULL, var = NULL, bhaz = NULL)
      else {
        temp <- rownames(rcfit[[i]]$gamma)
        rcfit[[i]] <- list(call = rcfit[[i]]$call,
                           coef = as.vector(rcfit[[i]]$gamma),
                           var = rcfit[[i]]$var.gamma,
                           bhaz = as.data.frame(rcfit[[i]]$cum))
        names(rcfit[[i]]$bhaz)[2] <- "intercept"
        names(rcfit[[i]]$coef) <- temp
      }
      rcfit[[i]]$call$data <- cl$data
    }
  }
  n <- nrow(mf)
  nadd <- length(iadd)
  nprp <- length(iprp)
  time <- maximalint(mf[, irsp])
  ntime <- nrow(time$int) - 1
  time$int <- time$int[1:ntime, ]
  A <- coxaalenic.ineq(mf[, iadd], ntime)
  ## set parameters controlling model fit
  control <- if (missing(control)) coxaalenic.control(...)
             else do.call(coxaalenic.control, control)
  ## initial parameter values
  if (is.null(init)) {
    init <- list()
    init$coef <- rep(0, nprp)
    init$bhaz <-
      rbind(time$int[, 2] / time$int[ntime, 2], matrix(0, nadd, ntime))
    if (rcinit & length(rcfit))
      if (!is.null(rcfit[[1]])) init$coef <- rcfit[[1]]$coef
  }
  init$rcinit <- rcinit
  if (close.cplex) on.exit(.C("freecplex"))
  fit <- .C("coxaalenic",
            coef = as.double(init$coef),
            bhaz = as.double(init$bhaz),
            as.integer(ntime),
            as.double(as.matrix(mf[, iprp])),
            as.integer(n),
            as.integer(nprp),
            as.double(as.matrix(cbind(1, mf[, iadd]))),
            as.integer(nadd + 1),
            as.integer(time$ind),
            as.double(A),
            as.integer(nrow(A)),
            as.double(control$eps),
            as.integer(control$iter.max),
            as.double(control$step.frac),
            as.double(control$step.scale),
            as.double(control$coef.typ),
            as.double(control$coef.max),
            as.integer(control$trace),
            as.integer(control$thread.max),
            var = as.double(rep(0, nprp^2)),
            loglik = as.double(rep(0, control$iter.max + 1)),
            iter = as.integer(0),
            fenchel = as.double(0),
            maxnorm = as.double(0),
            cputime = as.double(0),
            flag = as.integer(0))
  if (fit$flag == 1)
    stop("Parameter estimation failed; coefficient Hessian not invertible.")
  if (with(fit, any(is.na(coef), is.nan(coef), is.na(bhaz), is.nan(bhaz))))
    stop("Parameter estimation failed.")
  if (fit$flag == 2)
    stop("Variance estimation failed; profile information not invertible.")
  if (fit$flag == 3)
    stop("Variance estimation failed; profile maximizer not found.")
  if (with(fit, any(is.na(diag(var)), is.nan(diag(var)))))
    stop("Variance estimation failed.")
  if (with(fit, iter == control$iter.max & maxnorm > control$eps))
    warning("Maximum iterations reached before convergence.")
  names(fit$coef) <- names(mf)[iprp]
  var <- matrix(fit$var, nprp)
  rownames(var) <- colnames(var) <- names(mf)[iprp]
  bhaz <- cbind(time$int[, 2], t(matrix(fit$bhaz, nadd + 1)))
  bhaz <- as.data.frame(rbind(0, bhaz))
  names(bhaz) <- c("time", "intercept", names(mf[iadd]))
  censor.rate <- c(sum(mf[, irsp][, 1] == 0),
                   sum(mf[, irsp][, 1] > 0 & mf[, irsp][, 3] == 3),
                   sum(mf[, irsp][, 3] == 0)) / n
  names(censor.rate) <- c("left", "interval", "right")
  fit <- list(call = cl, n = n, p = nprp, coef = fit$coef, var = var,
              bhaz = bhaz, init = init, iter = fit$iter,
              loglik = n * fit$loglik[1:(fit$iter + 1)],
              fenchel = fit$fenchel, maxnorm = fit$maxnorm,
              cputime = fit$cputime, control = control, rcfit = rcfit,
              na.action = attr(mf, "na.action"), censor.rate = censor.rate,
              control = control)
  class(fit) <- "coxaalenic"
  fit
}
