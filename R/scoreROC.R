#' @title Area Under ROC Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic.
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#' glm_model <- glm(diabetes~., family=binomial,	data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes)
#' scoreROC(glm_au)
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
