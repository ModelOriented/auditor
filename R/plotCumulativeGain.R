#' @title Cumulative Gain Chart
#'
#' @description Cumulative Gain Chartis is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @import ROCR
#'
#'
#' @export


plotCumulativeGain <- function(object, ...){
  if(class(object)!="modelAudit") stop("plotCGains requires object class modelAudit.")
  rpp <- tpr <- label <- NULL
  df <- getCGainsDF(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getCGainsDF(resp) )
      }
    }
  }

  ggplot(df, aes(x = rpp, y = tpr, color = label)) +
    geom_line() +
    xlab("Rate of Positive Prediction") +
    ylab("True Positive Rate") +
    ggtitle("Cumulative Gain") +
    theme_light()
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


