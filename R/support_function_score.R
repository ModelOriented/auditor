confusionmatrix <- function(explainer, cutoff = 0.5) {

  yhat <- as.numeric(explainer$y_hat > cutoff)
  TP <- sum(yhat[yhat == 1] == explainer$y[yhat == 1])
  FP <- length(yhat[yhat == 1]) - TP
  TN <- sum(yhat[yhat == 0] == explainer$y[yhat == 0])
  FN <- length(yhat[yhat == 0]) - TN

  list(
    "TP" = TP,
    "FP" = FP,
    "TN" = TN,
    "FN" = FN
  )
}
