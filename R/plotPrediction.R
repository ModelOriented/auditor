#' @title Observed response values vs Predicted response
#'
#' @description Plot of predicted vs actual response.
#'
#'
#' @param object An object of class modelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotPrediction <- function(object, ...){
  observed <- predicted <- NULL

  df <- generatePredictionDF(object, variable)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, generatePredictionDF(resp, variable) )
      }
    }
  }

  ggplot(df, aes(observed, predicted, color = label)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    xlab("Observed response") +
    ylab("Predicted values") +
    ggtitle(paste0("Observed vs Predicted", variable)) +
    theme_light()
}


generatePredictionDF <- function(object, variable){
  resultDF <- data.frame(predicted = object$fitted.values, observed = object$y, label = object$label)
  return(resultDF)
}
