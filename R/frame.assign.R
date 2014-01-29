### expand model matrix `assign` attribute to give columns in model frame
frame.assign <- function(mf, mt = NULL, mm = NULL) {
  if (is.null(mt)) mt <- attr(mf, "terms")
  if (is.null(mm)) mm <- model.matrix(mt, mf)
  tlab <- attr(mt, "term.labels")
  ## ignore leading intercept term indexed by zero
  asgn <- attr(mm, "assign")[-1]
  ## account for intercept term in model frame
  asgn <- data.frame(cbind(match(tlab[asgn], names(mf)), asgn + 1))
  names(asgn) <- c("mf", "mm")
  asgn
}
