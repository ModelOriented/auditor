#' @title Area Under ROC Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic.
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @seealso \code{\link{plotROC}}
#'
#' @importFrom ROCR performance prediction
#'
#' @export


scoreROC <- function(object){

  predictionObj <- prediction(object$fitted.values, object$y)
  perf <- performance(predictionObj, measure = "auc")
  auc <- perf@y.values

  ROCResults <- list(
    name = "ROC",
    score = auc
  )

  class(ROCResults) <- "scoreAudit"
  return(ROCResults)
}
