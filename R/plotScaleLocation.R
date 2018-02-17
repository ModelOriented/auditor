#' @title Scale location plot
#'
#' @description Variable values vs square root of the absolute value of the residuals.
#' A vertical line corresponds to median.
#' Goldfeld-Quandt score - checking assumption of homoscedasticity of residuals.
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of dependent or independent variable to order residuals. If NULL the fitted values are taken.
#' @param score Logical, if TRUE value of \link{scoreGQ} will be added
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_smooth geom_vline geom_text
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#' @importFrom stats median
#'
#' @export
plotScaleLocation <- function(object, variable=NULL, score=TRUE){
  values <- sqrt.std.residuals <- group <- NULL
  if(is.null(variable)) variable <- "Fitted values"
  plotData <- generateScaleLocationDF(object, variable)


  p <- ggplot(plotData, aes(x = values, y = sqrt.std.residuals)) +
    geom_vline(aes(xintercept = median(plotData$values))) +
    geom_point() +
    geom_smooth(data=subset(plotData, group=="<med"),method = "loess", se = FALSE) +
    geom_smooth(data=subset(plotData, group==">med"),method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("\u221A|Standarized residuals|") +
    ggtitle("Scale Location") +
    theme_classic()

  if(score==TRUE){
    score <- scoreGQ(object)
    p <- p + geom_text(x = -Inf, y = Inf, label = paste("Score:", round(score$score,2)), hjust = -1, vjust = 1)
  }
  return(p)
}

generateScaleLocationDF <- function(object, variable){
  if(variable == "Fitted values") {
    values <- object$fitted.values
  } else {
    values <- object$data[,variable]
  }
  resultDF <- data.frame(values = values, std.residuals=object$std.residuals)
  resultDF <- dplyr::arrange(resultDF, values)
  resultDF$sqrt.std.residuals <- sqrt(abs(resultDF$std.residuals))
  n <- nrow(resultDF)
  resultDF$group <- rep(c("<med", ">med"), length.out=n, each = n/2)
  return(resultDF)
}
