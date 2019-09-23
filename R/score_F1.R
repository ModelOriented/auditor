#' @title F1 Score
#'
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
#' score_f1(exp_glm)
#'
#'
#' @export


score_f1 <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  conf <- confusionmatrix(object)

  ret <- (2 * (conf$TP / (conf$TP + conf$FP)) * (conf$TP / (conf$TP + conf$FN))) /
    (conf$TP / (conf$TP + conf$FN) + conf$TP / (conf$TP + conf$FP))
  F1_results <- list(
    name = "F1",
    score = ret
  )

  class(F1_results) <- "auditor_score"
  return(F1_results)
}


#' @title One Minus F1 Score
#'
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
#' score_one_minus_f1(exp_glm)
#'
#'
#' @export


score_one_minus_f1 <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  conf <- confusionmatrix(object)

  ret <- (2 * (conf$TP / (conf$TP + conf$FP)) * (conf$TP / (conf$TP + conf$FN))) /
    (conf$TP / (conf$TP + conf$FN) + conf$TP / (conf$TP + conf$FP))
  F1_results <- list(
    name = "one_minus_F1",
    score = 1 - ret
  )

  class(F1_results) <- "auditor_score"
  return(F1_results)
}
