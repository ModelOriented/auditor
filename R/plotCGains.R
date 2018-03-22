#' @title Cumulative Gains Chart
#'
#' @description Cumulative Gains Chartis a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#' @param object An object of class ModelAudit
#' @param newdata optionally, a data frame in which to look for variables with which to plot CGains curve. If omitted, the data used to build model will be used.
#' @param newy optionally, required if newdata used. Response vector for new data.
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @import ROCR
#'
#' @examples
#' library(auditor)
#' library(mlbench)
#' library(randomForest)
#' data("PimaIndiansDiabetes")
#'
#' model_rf <- randomForest(diabetes~., data=PimaIndiansDiabetes)
#' au_rf <- audit(model_rf, label="rf")
#' plotCGains(au_rf)
#'
#' model_glm <- glm(diabetes~., family=binomial,	data=PimaIndiansDiabetes)
#' au_glm <- audit(model_glm)
#' plotCGains(au_rf, au_glm)
#'
#' @export


plotCGains <- function(object, ..., newdata = NULL, newy){
  if(class(object)!="modelAudit") stop("plotCGains requires object class modelAudit.")
  rpp <- tpr <- label <- NULL
  df <- getCGainsDF(object, newdata, newy)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getCGainsDF(resp, newdata, newy) )
      }
    }
  }

  ggplot(df, aes(x = rpp, y = tpr, color = label)) +
    geom_line() +
    xlab("Rate of Positive Prediction") +
    ylab("True Positive Rate") +
    theme_light()
}

getCGainsDF <- function(object, newdata, newy){
  if (is.null(newdata)) {
    predictions <-   object$fitted.values
    y <- object$y
  } else {
    if(is.null(newy)) stop("newy must be provided.")
    predictions <- object$predict.function(object$model, newdata)
    y <- newy
  }

  pred <- prediction(predictions, y)
  gain <- performance(pred, "tpr", "rpp")

  res <- data.frame(rpp = gain@x.values[[1]], tpr = gain@y.values[[1]], alpha = gain@alpha.values[[1]],
                    label = object$label)
  return(res)
}
