#' @title Area Under ROC Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic.
#' @param object An object of class 'model_audit'.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm, y = titanic$survived)
#'
#' score_auc(audit_glm)
#'
#' @seealso \code{\link{plot_roc}}
#'
#' @export


score_auc <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")
  object <- model_evaluation(object)

  pred <- calculate_classif_evaluation(object$`_y_hat_`, object$`_y_`, object$`_label_`)
  pred_sorted <- pred[order(pred$`_y_hat_`, decreasing = TRUE), ]
  roc_y <- factor(pred_sorted$`_y_`)
  levels <- levels(roc_y)
  x = cumsum(roc_y == levels[1])/sum(roc_y == levels[1])
  y = cumsum(roc_y == levels[2])/sum(roc_y == levels[2])
  auc = sum((x[2:length(roc_y)]-x[1:length(roc_y)-1])*y[2:length(roc_y)])

  roc_results <- list(
    name = "roc",
    score = auc
  )

  class(roc_results) <- "auditor_score"
  return(roc_results)
}

#' @rdname score_auc
#' @export
scoreROC<- function(object) {
  message("Please note that 'scoreROC()' is now deprecated, it is better to use 'score_auc()' instead.")
  score_auc(object)
}
