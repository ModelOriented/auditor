yhat <- function(X.model, newdata, ...) {
  if ("lm" %in% class(X.model)) {
    stats::predict(X.model, newdata, ...)
  } else {
    as.numeric(stats::predict(X.model, newdata, ...))
  }
}
