#' @title Precision
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
#' score_precision(exp_glm)
#'
#'
#' @export


score_precision <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  conf <- confusionmatrix(object)
  ret <- conf$TP / (conf$TP + conf$FP)

  precision_results <- list(
    name = "precision",
    score = ret
  )

  class(precision_results) <- "auditor_score"
  return(precision_results)
}


#' @title One Minus Precision
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
#' score_one_minus_precision(exp_glm)
#'
#'
#' @export


score_one_minus_precision <- function(object){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  conf <- confusionmatrix(object)
  ret <- 1 - conf$TP / (conf$TP + conf$FP)

  precision_results <- list(
    name = "one_minus_precision",
    score = ret
  )

  class(precision_results) <- "auditor_score"
  return(precision_results)
}
