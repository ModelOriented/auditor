#' @title Create Model Evaluation explainer
#'
#' @description  Creates modelEvaluation object to be plotted. Model evaluation concentrates on classification models.
#'
#' @param object An object of class ModelAudit.
#' @param variable Optional. Name of variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm, y = titanic$survived)
#'
#' modelEvaluation(audit_glm)
#'
#'
#' @export
modelEvaluation <- function(object, variable = NULL){
  if(!("modelAudit" %in% class(object))) stop("The function requires an object created with audit().")

  result <- calculate_classif_evaluation(object$fitted.values, object$y, object$label)

    class(result) <- c("modelEvaluation", "data.frame")
  return(result)
}



calculate_classif_evaluation <- function(predictions, y, label){

  y <- factor(y)
  levels <- levels(y)
  pos_label <- levels[2]
  neg_label <- levels[1]

  pred <- data.frame(predictions = predictions, y = y)

  pred_sorted <- pred[order(pred$predictions, decreasing = TRUE), ]
  # true positives & false negatives
  tp <- cumsum(pred_sorted$y == pos_label)
  fp <- cumsum(pred_sorted$y == neg_label)
  # cutoffs aka thresholds aka alpha
  cutoffs <- pred_sorted$predictions
  # number of positives & negatives
  n_pos <- sum(y == levels[2] )
  n_neg <- sum(y == levels[1] )
  # false negatives & true negatives
  fn <- n_pos - tp
  tn <- n_neg - fp
  # number of positive predistions & number of negative predictions
  n_pos_pred <- tp + fp
  n_neg_pred <- fn + tn
  # true positive rate & false positive rate
  tpr <- tp / n_pos
  fpr <- fp / n_neg
  # rate of positive predictions
  rpp <- (tp + fp) / (tp +fp +tn +fn)
  res <- data.frame(fitted.values = predictions,
             y = y,
             cutoffs = cutoffs,
             tpr = tpr,
             fpr = fpr,
             rpp = rpp,
             tp = tp)
  res$label <- factor(label)
  res
}
