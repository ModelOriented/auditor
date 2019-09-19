#' @title One minus Area Under ROC Curve (AUC)
#'
#' @description One minus Area Under Curve (AUC) for Receiver Operating Characteristic.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @return An object of class 'auditor_score'.
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- titanic$survived == "yes"
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' #create an explainer
#' exp_glm <- DALEX::explain(model_glm, y = titanic$survived)
#'
#' # calculate score
#' score_one_minus_auc(exp_glm)
#'
#'
#' @export


score_one_minus_auc <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  ret <- 1 - score_auc(object)$score
  roc_results <- list(
    name = "one_minus_auc",
    score = ret
  )

  class(roc_results) <- "auditor_score"
  return(roc_results)
}
