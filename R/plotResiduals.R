#' @title Plot Residuals vs Fitted or Variable Values
#'
#' @description A plot of residuals against fitted values or any variable values.
#'
#'
#' @param object An object of class modelAudit
#' @param variable name of modle variable for x-axis. If NULL fitted values are taken.
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotResiduals <- function(object, variable=NULL){
  residuals <- values <- NULL
  if(is.null(variable)) variable <- "Fitted values"
  plotData <- generateResidualsDF(object, variable)

  ggplot(plotData, aes(values, residuals)) +
    geom_point() +
    geom_smooth(method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("Residuals") +
    ggtitle(paste0("Residuals vs ", variable)) +
    theme_light()
}


generateResidualsDF <- function(object, variable){
  if(variable == "Fitted values") {
    values <- object$fitted.values
  } else {
    values <- object$data[,variable]
  }
  n <- length(object$residuals)
  resultDF <- data.frame(values = values, residuals = object$residuals)
  resultDF <- dplyr::arrange(resultDF, values)
  return(resultDF)
}
