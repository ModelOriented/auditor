#' @title Autocorrelation plot
#'
#' @description Plot i-th residual vs i+1-th residual. Checking autocorrelation of residuals.
#' Runs score, Durbin-Watson score
#'
#'
#' @param object An object of class ModelAudit
#' @param variable name of dependent or independent variable to order residuals. If NULL the fitted values are taken.
#'
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom ggplot2 geom_point geom_hline
#' @importFrom ggplot2 ggtitle xlab ylab
#' @importFrom ggplot2 theme_classic
#'
#' @export
plotAutocorrelation <- function(object, variable=NULL){
  x <- y <- NULL
  if(is.null(variable)) variable <- "Fitted values"
  plotData <- generateAutocorrelationDF(object, variable)

  ggplot(plotData, aes(x, y)) +
    geom_point() +
    geom_hline(yintercept = 0) +
    xlab("residual i") +
    ylab("residual i+1") +
    ggtitle("Autocorrelation") +
    theme_classic()
}


generateAutocorrelationDF <- function(object, variable){
  if(variable == "Fitted values") {
    values <- object$fitted.values
  } else {
    values <- object$data[,variable]
  }
  n <- length(object$residuals)
  tmpDF <- data.frame(values = values, x = object$residuals)
  tmpDF <- dplyr::arrange(tmpDF, values)
  resultDF <- data.frame(x = tmpDF$x[-n], y = tmpDF$x[-1])
  return(resultDF)
}
