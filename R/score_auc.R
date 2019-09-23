#' @title Area Under ROC Curve (AUC)
#'
#' @description Area Under Curve (AUC) for Receiver Operating Characteristic.
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
#' score_auc(exp_glm)
#'
#' @seealso \code{\link{plot_roc}}
#'
#' @export


score_auc <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")
  object <- model_evaluation(object)
  pred <- data.frame(y_hat = object$`_y_hat_`,
                     y = object$`_y_`)
  pred_sorted <- pred[order(pred$y_hat, decreasing = TRUE), ]
  roc_y <- factor(pred_sorted$y)
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

#' @rdname score_auc
#' @export
scoreROC<- function(object) {
  message("Please note that 'scoreROC()' is now deprecated, it is better to use 'score_auc()' instead.")
  score_auc(object)
}
