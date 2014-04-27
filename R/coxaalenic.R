coxaalenic <- function(formula, data = parent.frame(), subset, init = NULL,
                       formula.timereg = NULL, init.timereg = FALSE,
                       close.cplex = TRUE, control, ...)
{
  ## extract model frame and perform input validation
  cl <- match.call(expand.dots = FALSE)
  datargs <- c("formula", "data", "subset")
  mf <- cl[c(1, match(datargs, names(cl), nomatch = 0))]
  mf[[1]] <- as.name("model.frame")
  specials <- c("prop", "strata", "cluster", "tt")
  ftrm <- if (missing(data)) terms(formula, specials)
          else terms(formula, data = data, specials)
  if (with(attr(ftrm, "specials"), length(c(strata, cluster, tt))))
    stop("The 'strata', 'cluster' and 'tt' terms not supported")
  ## store column indices of terms in model frame
  irsp <- attr(ftrm, "response")
  iprp <- attr(ftrm, "specials")$prop
  iadd <- (1:(length(attr(ftrm, "variables")) - 1))[-c(irsp, iprp)]
  if (!length(iprp)) stop("Model requires 'prop' terms")
  mf$formula <- ftrm
  mf$na.action <- as.name("na.omit")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  mm <- model.matrix(mt, mf)
  ## find proportional and additive terms in model matrix
  asgn <- frame.assign(mf, mt, mm)
  jprp <- subset.data.frame(asgn, frame %in% iprp)$matrix
  ## additive term always includes intercept
  jadd <-
    if (length(iadd)) c(1, subset.data.frame(asgn, frame %in% iadd)$matrix)
    else 1
  if (!inherits(mf[, irsp], "Surv")) stop("Response is not a 'Surv' object")
  ## Surv converts interval2 to interval
  if (attr(mf[, irsp], "type") != "interval"
      | !(all(mf[, irsp][, 3] %in% c(0, 3))))
    stop("Response is not an 'interval2'-type 'Surv' object")
  ## fit right-censored data alternatives with timereg's cox.aalen
  fit.timereg <- list()
  if (!is.null(formula.timereg)) {
    if (!missing(subset)) warning("Model alternatives not based on subset")
    keep <- 1:nrow(data)
    if (!is.null(omit <- attr(mf, "na.action"))) keep <- keep[-omit]
    for (i in 1:length(formula.timereg)) {
      fit.timereg[[i]] <- cl
      fit.timereg[[i]]$formula <-
        update.formula(fit.timereg[[i]]$formula, formula.timereg[[i]])
      ## cox.aalen arguments, constructed to avoid NA-related errors
      temp <-
        list(formula = fit.timereg[[i]]$formula, data = data[keep, ],
             robust = 0, silent = 1, max.timepoint.sim = nrow(data))
      invisible(capture.output(fit.timereg[[i]] <-
                               try(do.call("cox.aalen", temp))))
      if (inherits(fit.timereg[[i]], "try-error"))
        fit.timereg[[i]] <-
          list(call = temp, n = NULL, coef = NULL, var = NULL, bhaz = NULL)
      else {
        temp <- rownames(fit.timereg[[i]]$gamma)
        fit.timereg[[i]] <- list(call = fit.timereg[[i]]$call,
                                 n = length(keep),
                                 coef = as.vector(fit.timereg[[i]]$gamma),
                                 var = fit.timereg[[i]]$var.gamma,
                                 bhaz = as.data.frame(fit.timereg[[i]]$cum))
        names(fit.timereg[[i]]$bhaz)[2] <- "intercept"
        names(fit.timereg[[i]]$coef) <- temp
      }
      fit.timereg[[i]]$call$data <- cl$data
    }
  }
  n <- nrow(mf)
  nadd <- length(jadd)
  nprp <- length(jprp)
  time <- maximalint(mf[, irsp])
  ntime <- nrow(time$int) - 1
  time$int <- time$int[1:ntime, ]
  A <- coxaalenic.ineq(mm[, jadd[-1]], ntime)
  ## set parameters controlling model fit
  control <- if (missing(control)) coxaalenic.control(...)
             else do.call(coxaalenic.control, control)
  ## initial parameter values
  if (is.null(init)) init <- list()
  if (is.null(init$coef)) init$coef <- rep(0, nprp)
  if (is.null(init$bhaz)) {
    init$bhaz <-
      rbind(time$int[, 2] / time$int[ntime, 2], matrix(0, nadd - 1, ntime))
    if (init.timereg & length(fit.timereg))
      if (!is.null(fit.timereg[[1]])) init$coef <- fit.timereg[[1]]$coef
  }
  init$init.timereg <- init.timereg
  if (close.cplex) on.exit(.C("freecplex"))
  fit <- .C("coxaalenic",
            coef = as.double(init$coef),
            bhaz = as.double(init$bhaz),
            as.integer(ntime),
            as.double(as.matrix(mm[, jprp])),
            as.integer(n),
            as.integer(nprp),
            as.double(as.matrix(mm[, jadd])),
            as.integer(nadd),
            as.integer(time$ind),
            as.double(A),
            as.integer(nrow(A)),
            as.double(control$eps),
            as.integer(control$eps.norm == "max"),
            as.integer(control$iter.max),
            as.double(control$armijo),
            as.double(control$coef.typ),
            as.double(control$coef.max),
            as.integer(control$trace),
            as.integer(control$thread.max),
            var = as.double(rep(0, nprp^2)),
            loglik = as.double(rep(0, control$iter.max + 1)),
            iter = as.integer(0),
            maxnorm = as.double(0),
            gradnorm = as.double(0),
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
  names(fit$coef) <- colnames(mm)[jprp]
  var <- matrix(fit$var, nprp)
  rownames(var) <- colnames(var) <- colnames(mm)[jprp]
  bhaz <- cbind(time$int[, 2], t(matrix(fit$bhaz, nadd)))
  bhaz <- as.data.frame(rbind(0, bhaz))
  names(bhaz) <- c("time", colnames(mm)[jadd])
  censor.rate <- matrix(c(sum(mf[, irsp][, 1] == 0),
                          sum(mf[, irsp][, 1] > 0 & mf[, irsp][, 3] == 3),
                          sum(mf[, irsp][, 3] == 0)) / n, nrow = 1)
  dimnames(censor.rate) <-
    list("Censoring rate", c("Left", "Interval", "Right"))
  fit <- list(call = cl, n = n, p = nprp, coef = fit$coef, var = var,
              basehaz = bhaz, init = init,
              loglik = n * fit$loglik[1:(fit$iter + 1)], iter = fit$iter,
              maxnorm = fit$maxnorm, gradnorm = fit$gradnorm,
              cputime = fit$cputime, fit.timereg = fit.timereg,
              na.action = attr(mf, "na.action"), censor.rate = censor.rate,
              control = control)
  class(fit) <- c("coxaalenic", "coxinterval")
  fit
}
