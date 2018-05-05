#' @title Area Under ROC Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic
#' @param object An object of class ModelAudit
#'
#' @return an object of class scoreAudit
#'
#' @seealso \code{\link{plotROC}}
#'
#' @importFrom ModelMetrics auc
#'
#' @export


scoreROC <- function(object){

  auc <- auc(object$y, object$fitted.values)

  ROCResults <- list(
    name = "ROC",
    score = auc
  )

  class(ROCResults) <- "scoreAudit"
  return(ROCResults)
}
