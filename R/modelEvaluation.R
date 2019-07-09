#' @title Create Model Evaluation explainer
#'
#' @description  Creates modelEvaluation object to be plotted. Model evaluation concentrates on classification models.
#'
#' @param object An object of class ModelAudit.
#' @param variable Optional. Name of variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @examples
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#' glm_model <- glm(diabetes~., family=binomial,	data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes)
#'
#' modelEvaluation(glm_au)
#'
#'
#' @export
modelEvaluation <- function(object, variable = NULL){
  if(!("modelAudit" %in% class(object))) stop("The function requires an object created with audit().")

  result <- calculate_classif_evaluation(object$fitted.values, object$y)

  idealCGainsDF <- getidealCGainsDF(object)[-1,]

    class(result) <- c("modelEvaluation", "data.frame")
    attr(result,'idealCGains') <- idealCGainsDF
  return(result)
}

calculate_classif_evaluation <- function(predictions, y){

  y <- factor(y)
  levels <- levels(y)
  pos_label <- levels[2]
  neg_label <- levels[1]

  pred <- data.frame(predictions = predictions, y = y)

  pred_sorted <- pred[order(pred$predictions, decreasing = TRUE), ]

  tp <- cumsum(pred_sorted$y == pos_label)
  fp <- cumsum(pred_sorted$y == neg_label)


  cutoffs <- c(Inf, pred_sorted$predictions)

  n_pos <- sum(y == levels[2] )
  n_neg <- sum(y == levels[1] )

  fn <- n_pos - tp
  tn <- n_neg - fp

  n_pos_pred <- tp + fp
  n_neg_pred <- fn + tn

  tpr <- tp / n_pos
  fpr <- fp / n_neg

  rpp <- (tp + fp) / (tp +fp +tn +fn)

  data.frame(fitted.values = c(1, predictions), y = c(1, y), cutoffs = cutoffs, tpr = c(1, tpr), fpr = c(1, fpr), rpp = c(1, rpp), tp = c(1, tp))
}



getidealCGainsDF <- function(object){

  predictions <- object$y
  y <- as.numeric(as.character(object$y))

  res <- calculate_classif_evaluation(predictions, y)
  res <- cbind(res, label = "ideal")

  return(res)
}
