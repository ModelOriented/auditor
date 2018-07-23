#' @title Create Model Evaluation explainer
#'
#' @description  Creates modelEvaluation object to be plotted. Model evaluation concentrates on classification models.
#'
#' @param object An object of class ModelAudit.
#' @param variable Optional. Name of variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @export
modelEvaluation <- function(object, variable = NULL){
  if(!("modelAudit" %in% class(object))) stop("The function requires an object created with audit().")

  CGainsDF <- getCGainsDF(object)[-1,]

  result <- data.frame(
    y=object$y,
    fitted.values = object$fitted.values,
    rpp = CGainsDF$rpp,
    tpr = CGainsDF$tpr,
    alpha = CGainsDF$alpha,
    label=object$label)

    class(result) <- c("modelEvaluation", "data.frame")

  return(result)
}


getCGainsDF <- function(object){

  predictions <- object$fitted.values
  y <- as.numeric(as.character(object$y))

  pred <- prediction(predictions, y)
  gain <- performance(pred, "tpr", "rpp")

  res <- data.frame(rpp = gain@x.values[[1]], tpr = gain@y.values[[1]], alpha = gain@alpha.values[[1]],
                    label = object$label)
  return(res)
}

