#' @title Half-Normal plot
#'
#' @description Function \code{plotHalfNormal}...
#'
#' @param object fitted model object or numeric vector
#' @param xlab the text for the x axis
#' @param ylab the text for the y axis
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#'
#' @return An object of class ggplot
#'
#' @importFrom hnp hnp
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab
#'
#' @export

plotHalfNormal <- function(object, xlab = "Theoretical quantiles", ylab = "residuals", ...){
  x <- residuals <- upper <- lower <- NULL
  hnpObject <- hnp(object, plot.sim=FALSE, ...)
  dataPlot <- data.frame(x = hnpObject$x, lower = hnpObject$lower,
                         median = hnpObject$median, upper = hnpObject$upper,
                         residuals = hnpObject$residuals)
  ggplot(dataPlot, aes(x = x)) +
    geom_point(aes(y = residuals)) +
    geom_line(aes(y=upper))+
    geom_line(aes(y=lower))+
    geom_line(aes(y=median), linetype = 2) +
    xlab(xlab) +
    ylab(ylab)
}

