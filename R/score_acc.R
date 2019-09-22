#' @title Accuracy
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
#' score_acc(exp_glm)
#'
#'
#' @export


score_acc <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  conf <- confusionmatrix(object)
  ret <- (conf$TP + conf$TN) / (conf$TP + conf$FP + conf$TN + conf$FN)

  acc_results <- list(
    name = "acc",
    score = ret
  )

  class(acc_results) <- "auditor_score"
  return(acc_results)
}


#' @title One minus accuracy
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
#' score_one_minus_acc(exp_glm)
#'
#'
#' @export


score_one_minus_acc <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  conf <- confusionmatrix(object)
  ret <- 1 - (conf$TP + conf$TN) / (conf$TP + conf$FP + conf$TN + conf$FN)

  acc_results <- list(
    name = "one_minus_acc",
    score = ret
  )

  class(acc_results) <- "auditor_score"
  return(acc_results)
}
