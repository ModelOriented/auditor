#' @title Half-Normal plot
#'
#' @description Function \code{plotHalfNormal}...
#'
#' @param object fitted model object or numeric vector
#' @param score if TRUE score based on probability density function
#' @param quant.scale if TRUE values on avis are on quantile scale
#' @param main title of plot
#' @param xlab the text for the x axis
#' @param ylab the text for the y axis
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#'
#' @details TO DO write about halfplots and scores.
#'
#' @return An object of class ggplot
#'
#' @importFrom hnp hnp
#' @importFrom ggplot2 ggplot aes geom_point geom_line xlab ylab annotate scale_x_continuous scale_y_continuous ggtitle coord_fixed
#'
#' @export

plotHalfNormal <- function(object, score=TRUE, quant.scale=FALSE,
                           xlab = "Half-Normal Quantiles", ylab = "Residuals",
                           main = "", ...){
  x <- residuals <- upper <- lower <- NULL
  hnpObject <- halfNormal(object,...)

  dataPlot <- datasetHalfNormalPlot(hnpObject, quant.scale)

  p <- ggplot(dataPlot, aes(x = x)) +
    geom_point(aes(y = residuals)) +
    geom_line(aes(y=upper))+
    geom_line(aes(y=lower))+
    geom_line(aes(y=median), linetype = 2) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    xlab(xlab) +
    ylab(ylab) +
    ggtitle(main) +
    theme_classic()

  if(quant.scale==TRUE) {
    p <- p +
      scale_x_continuous(limits=c(0,1)) +
      scale_y_continuous(limits=c(0,1)) +
      coord_fixed(ratio = 1)
  }
  if(score==TRUE) {
    envScore <- calculateScorePDF(hnpObject)
    p <- p + annotate("text", x = max(dataPlot$x)/4, y = max(dataPlot$residuals)*3/4, label = paste("Score:",round(envScore,2)))
  }
  return(p)
}

#' Calculating simulated residuals and envelope
#' @usage NULL
halfNormal <- function(object, ...){
trace(hnp::.makehnp, at = 13, print = FALSE,
      tracer = quote(simdata <- list(
        "x"=q.x,
        "lower"=t(env)[, 1],
        "median"=t(env)[, 2],
        "upper"=t(env)[, 3],
        "residuals"=res.original, "simresiduals"=res) ) )


  hnpObject <- hnp(object, plot.sim=FALSE, ...)
}




#' Creating dataset for Half-Normal Plot
#' @usage NULL
#' @importFrom fdrtool phalfnorm
#' @importFrom stats ecdf
datasetHalfNormalPlot <- function(hnpObject, quant.scale){
  n <- length(hnpObject$residuals)

  if (quant.scale == FALSE) {
    dataPlot <- data.frame(x = hnpObject$x, lower = hnpObject$lower,
                           median = hnpObject$median, upper = hnpObject$upper,
                           residuals = hnpObject$residuals)
  } else {
    quantilesResiduals <-  seq(0,1,length.out = n)
    quantilesTheoretical <- phalfnorm(hnpObject$residuals)
    invQuantile <- ecdf(hnpObject$residuals)
    quantilesUpper <- invQuantile(hnpObject$upper)
    quantilesMedian <- invQuantile(hnpObject$median)
    quantilesLower <- invQuantile(hnpObject$lower)
    dataPlot <- data.frame(x = quantilesTheoretical, lower = quantilesLower,
                           median = quantilesMedian, upper = quantilesUpper,
                           residuals = quantilesResiduals)
  }
  return(dataPlot)
}

#' Calculating Liklehood for each residual
#' @usage NULL
#' @importFrom stats dnorm density
calculateKDE <- function(res, simres){
  simres <- as.numeric(simres)
  (length(simres)/2 - abs(sum(res<simres) - length(simres)/2)+1)/(length(simres)/2)
}


#' Calculating PDF score
#' @usage NULL
calculateScorePDF <- function(hnpObject){
  res <- hnpObject$residuals
  simres <- as.data.frame(t(hnpObject$simresiduals))
  n <- length(res)
  PDFs <- mapply(calculateKDE, res, simres)
  return(sum(log(PDFs)))
}





