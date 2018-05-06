#' @title Autocorrelation Plot
#'
#' @description Plot of i-th residual vs i+1-th residual.
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of dependent or independent variable to order residuals. If NULL original data order is taken.
#' @param score Logical, if TRUE values of \link{scoreDW} and \link{scoreRuns} will be added to plot.
#'
#' @import ggplot2
#'
#' @export
plotAutocorrelation <- function(object, variable=NULL, score=TRUE){
  x <- y <- NULL
  plotData <- generateAutocorrelationDF(object, variable)

  p <- ggplot(plotData, aes(x, y)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE) +
      geom_hline(yintercept = 0) +
      xlab("residual i") +
      ylab("residual i+1") +
      ggtitle("Autocorrelation plot") +
      theme_light()

  if(score==TRUE){
    score1 <- scoreDW(object, variable)
    score2 <- scoreRuns(object, variable)
    p <- p + geom_text(x = -Inf, y = Inf,
                       label = paste("Durbin-Watson Score:", round(score1$score,2), " Runs Score:", round(score2$score,2)),
                       hjust = -1, vjust = 1)
  }

  return(p)
}


generateAutocorrelationDF <- function(object, variable){
  if(is.null(variable) || variable == "Fitted values") {
    values <- object$fitted.values
  } else {
    values <- object$data[,variable]
  }
  n <- length(object$residuals)
  tmpDF <- data.frame(values = values, x = object$residuals)
  if(!is.null(variable)){
    tmpDF <- dplyr::arrange(tmpDF, values)
  }
  resultDF <- data.frame(x = tmpDF$x[-n], y = tmpDF$x[-1])
  return(resultDF)
}
