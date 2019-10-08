#' @title Accuracy
#'
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calcuate the score. Pass \code{NULL} if you want to use \code{data} from \code{object}.
#'
#' @return An object of class \code{auditor_score}.
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
#' @rdname score_acc
score_acc <- function(object, data = NULL) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)) object$data <- data

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
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calcuate the score. Pass \code{NULL} if you want to use \code{data} from \code{object}.
#'
#' @return An object of class \code{auditor_score}.
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
#' @rdname score_one_minus_acc
score_one_minus_acc <- function(object, data = NULL) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)) object$data <- data

  conf <- confusionmatrix(object)
  ret <- 1 - (conf$TP + conf$TN) / (conf$TP + conf$FP + conf$TN + conf$FN)

  acc_results <- list(
    name = "one_minus_acc",
    score = ret
  )

  class(acc_results) <- "auditor_score"
  return(acc_results)
}
