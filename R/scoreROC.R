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
#'
#' @export


scoreROC <- function(object){
  if(!("modelEvaluation" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if("modelAudit" %in% class(object)) object <- modelEvaluation(object)

  pred <- calculate_classif_evaluation(object$fitted.values, object$y, object$label)
  pred_sorted <- pred[order(pred$fitted.values, decreasing = TRUE), ]
  roc_y <- factor(pred_sorted$y)
  levels <- levels(roc_y)
  x = cumsum(roc_y == levels[1])/sum(roc_y == levels[1])
  y = cumsum(roc_y == levels[2])/sum(roc_y == levels[2])
  auc = sum((x[2:length(roc_y)]-x[1:length(roc_y)-1])*y[2:length(roc_y)])

  ROCResults <- list(
    name = "ROC",
    score = auc
  )

  class(ROCResults) <- "scoreAudit"
  return(ROCResults)
}
