#' @title Area under precision-recall curve
#'
#' @description Area under precision-recall (AUPRC) curve.
#'
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calculate the score.
#'  Pass \code{NULL} if you want to use \code{data} from \code{object}.
#' @param y New y parameter will be used to calculate score.
#' @param ... Other arguments dependent on the type of score.
#'
#' @return An object of class \code{auditor_score}.
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' # create an explainer
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # calculate score
#' score_auprc(glm_audit)
#'
#'
#' @export
score_auprc <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)) {
    object$data <- data
    object$y <- y
    object$y_hat <- object$predict_function(object$model, data)
  }

  object <- model_evaluation(object)
  pred <- data.frame(y_hat = object$`_y_hat_`,
                     y = object$`_y_`)
  pred_sorted <- pred[order(pred$y_hat, decreasing = TRUE), ]
  roc_y <- factor(pred_sorted$y)

  positive_label <- levels(roc_y)[2]
  negative_label <- levels(roc_y)[1]

  positive_num <- sum(pred_sorted$y == positive_label)


  tp <- cumsum(pred_sorted$y == positive_label)
  fp <- cumsum(pred_sorted$y == negative_label)
  # remove duplicates
  duplicates <- rev(duplicated(rev(pred_sorted$y_hat)))
  tp <- c(0, tp[!duplicates])
  fp <- c(0, fp[!duplicates])

  fn <- nrow(pred_sorted) - tp

  precision <- tp / (tp + fp)
  recall <- tp / positive_num


  xroc <- recall
  yroc <- precision

  auprc <- sum( 0.5* (xroc[2:length(xroc)]-xroc[1:length(xroc)-1])* (yroc[2:length(xroc)] +yroc[1:length(xroc)-1]), na.rm = TRUE )

  results <- list(
    name = "auprc",
    score = auprc
  )

  class(results) <- "auditor_score"
  return(results)
}



#' @title One Minus area under precision-recall curve
#'
#' @description One Minus Area under precision-recall (AUPRC) curve.
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param data New data that will be used to calculate the score.
#'  Pass \code{NULL} if you want to use \code{data} from \code{object}.
#' @param y New y parameter will be used to calculate score.
#' @param ... Other arguments dependent on the type of score.
#'
#' @return An object of class \code{auditor_score}.
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' # create an explainer
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # calculate score
#' score_one_minus_auprc(glm_audit)
#'
#'
#' @export
score_one_minus_auprc <- function(object, data = NULL, y = NULL, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  # inject new data to the explainer
  if (!is.null(data)) object$data <- data

  auprc <- score_auprc(object, data, y, ...)$score

  results <- list(
    name = "one_minus_auprc",
    score = 1 - auprc
  )

  class(results) <- "auditor_score"
  return(results)
}
