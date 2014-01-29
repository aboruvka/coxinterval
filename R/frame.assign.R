### expand 'assign' attribute to indicate column index in both the model frame
### and the model matrix
frame.assign <- function(frame, terms = NULL, matrix = NULL) {
  if (is.null(terms)) terms <- attr(frame, "terms")
  if (is.null(matrix)) matrix <- model.matrix(terms, frame)
  labels <- attr(terms, "term.labels")
  ## account for intercept term in model frame
  assign <- c(0, match(labels[attr(matrix, "assign")], names(frame)))
  assign <- data.frame(cbind(assign, 1:ncol(matrix)))
  names(assign) <- c("frame", "matrix")
  assign
}
