#' @title Predicted response vs Observed or Variable Values
#'
#' @description Plot of predicted response vs observed or variable Values.
#'
#'
#' @param object An object of class modelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotPrediction <- function(object, ..., variable = "Observed response"){
  values <- predicted <- label <- NULL

  df <- generatePredictionDF(object, variable)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, generatePredictionDF(resp, variable) )
      }
    }
  }

  maybeVS <- ifelse(is.null(variable), "", "vs")

  p <- ggplot(df, aes(values, predicted, color = label)) +
          geom_point() +
          xlab(variable) +
          ylab("Predicted values") +
          ggtitle(paste("Predicted", maybeVS, variable)) +
          theme_light()

  if(!is.null(variable) && variable == "Observed response") p <- p + geom_abline(slope = 1, intercept = 0)

  return(p)
}


generatePredictionDF <- function(object, variable){
  if(!is.null(variable)){
    if (variable == "Observed response") {
      values <- object$y
    } else {
      values <- object$data[,variable]
    }
  } else {
    values <- 1:length(object$residuals)
  }
  resultDF <- data.frame(predicted = object$fitted.values, values = values, label = object$label)
  return(resultDF)
}
