#' @title Half-Normal plot
#'
#' @description The half-normal plot is one of the tools designed to evaluate the goodness of fit of a
#' statistical models. It is a graphical method for comparing two probability distributions by plotting
#' their quantiles against each other.
#' Points on the plot correspond to ordered absolute values of model diagnostic
#' (i.e. standardized residuals) plotted against theoretical order statistics from a half-normal distribution.
#'
#' @param object modelAudit object, modelFit object.
#' @param score If TRUE score based on probability density function is displayed on the plot.
#' @param quant.scale if TRUE values on axis are on quantile scale.
#' @param main Title of plot.
#' @param xlab The text for the x axis.
#' @param ylab The text for the y axis.
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#'
#' @return An object of class ggplot
#'
#' @import ggplot2
#' @importFrom hnp hnp
#' @importFrom fdrtool phalfnorm
#' @importFrom stats ecdf dnorm density
#'
#' @seealso \code{\link{scoreHalfNormal}}
#'
#' @export

plotHalfNormal <- function(object, score=TRUE, quant.scale=FALSE,
                           xlab = "half-normal Quantiles", ylab = "residuals",
                           main = "", ...){
  if("modelAudit" %in% class(object)) {
    object <- modelFit(object, quant.scale = quant.scale, ...)
  }

  x <- residuals <- upper <- lower <- NULL

  p <- ggplot(object, aes(x = x)) +
    geom_point(aes(y = residuals)) +
    geom_line(aes(y=upper))+
    geom_line(aes(y=lower))+
    geom_line(aes(y=median), linetype = 2) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main) +
    theme_light()

  if(quant.scale==TRUE) {
    p <- p +
      scale_x_continuous(limits=c(0,1)) +
      scale_y_continuous(limits=c(0,1)) +
      coord_fixed(ratio = 1)
  }
  if(score==TRUE) {
    envScore <- calculateScorePDF(object)
    p <- p + geom_text(x = -Inf, y = Inf, label = paste("Score:",round(envScore,2)), hjust = -1, vjust = 1)

  }
  return(p)
}

# Calculating Likelihood for each residual
calculateKDE <- function(res, simres){
  simres <- as.numeric(simres)
  (length(simres)/2 - abs(sum(res<=simres) - length(simres)/2))/(length(simres)/2)
}


# Calculating PDF score
calculateScorePDF <- function(hnpObject){
  res <- hnpObject$residuals
  simres <- as.data.frame(t(hnpObject[,6:ncol(hnpObject)]))
  n <- length(res)
  PDFs <- mapply(calculateKDE, res, simres)
  return(sum(PDFs))
}

