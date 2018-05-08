#' @title Scale location plot
#'
#' @description Variable values vs square root of the absolute value of the residuals.
#' A vertical line corresponds to median.
#'
#'
#' @param object An object of class ModelAudit.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param score A logical value. If TRUE value of \link{scoreGQ} will be added.
#'
#' @import ggplot2
#' @importFrom stats median
#'
#' @export
plotScaleLocation <- function(object, variable=NULL, score=FALSE){
  values <- sqrt.std.residuals <- group <- NULL
  plotData <- generateScaleLocationDF(object, variable)

  p <- ggplot(plotData, aes(x = values, y = sqrt.std.residuals)) +
    geom_vline(aes(xintercept = median(plotData$values))) +
    geom_point() +
    geom_smooth(data=subset(plotData, group=="<med"),method = "loess", se = FALSE) +
    geom_smooth(data=subset(plotData, group==">med"),method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("\u221A|Standarized residuals|") +
    ggtitle("Scale Location") +
    theme_light()

  if(score==TRUE){
    score <- scoreGQ(object, variable)
    p <- p + geom_text(x = -Inf, y = Inf, label = paste("Score:", round(score$score,2)), hjust = -1, vjust = 1)
  }
  return(p)
}

generateScaleLocationDF <- function(object, variable){

  resultDF <- data.frame(std.residuals=object$std.residuals)
  n <- nrow(resultDF)

  if(!is.null(variable)){
    if((variable == "Predicted response") || (variable == "Fitted values")) {
      values <- object$fitted.values
    } else {
      values <- object$data[,variable]
    }
  } else {
    values <- 1:n
  }
  resultDF$values <- values

  resultDF <- resultDF[order(values),]
  resultDF$sqrt.std.residuals <- sqrt(abs(resultDF$std.residuals))

  resultDF$group <- rep(c("<med", ">med"), length.out=n, each = n/2)
  return(resultDF)
}
