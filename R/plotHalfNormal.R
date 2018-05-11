#' @title Half-Normal plot
#'
#' @description The half-normal plot is one of the tools designed to evaluate the goodness of fit of a
#' statistical models. It is a graphical method for comparing two probability distributions by plotting
#' their quantiles against each other.
#' Points on the plot correspond to ordered absolute values of model diagnostic
#' (i.e. standardized residuals) plotted against theoretical order statistics from a half-normal distribution.
#'
#' @param object ModelAudit object, fitted model object or numeric vector.
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
  data <- NULL

  if(class(object)=="modelAudit") {
    object <- object$model
    data <- object$data
  }

  if("randomForest" %in% class(object)) {
    if(is.null(data)) data <- model.frame(object)
    p <- plotHN.randomForest(object, data, quant.scale=quant.scale, ...)
    return(p)
    }
  plotHN.default(object, quant.scale=quant.scale, ...)
}


plotHN.randomForest <- function(object, data, ...){
  d.fun <- function(obj){
    1 - predict(obj, type = "prob")[cbind(1:length(obj$y),obj$y)]
  }
  s.fun <- function(n, obj){
    probs <- predict(obj, type = "prob")
    yi <- apply(probs, 1, FUN = function(x)sample(x=obj$classes, 1, prob = x))
    as.factor(yi)
  }
  f.fun <- function(y.) {
    newdata <- data
    newdata[,as.character(object$terms[[2]])] <- as.factor(y.)
    mod <- update(object, data = newdata)
    return(mod)
  }
    hnpObject <- hnp(object, newclass = TRUE, diagfun = d.fun, simfun = s.fun, fitfun = f.fun)

    dataPlot <- datasetHalfNormalPlot(hnpObject, ...)

    plotIt(hnpObject, dataPlot, ...)
}

plotHN.default <- function(object, ...){

  hnpObject <- hnp(object, plot.sim=FALSE, ...)

  dataPlot <- datasetHalfNormalPlot(hnpObject, ...)

  plotIt(hnpObject, dataPlot, ...)
}




plotIt <- function(hnpObject, dataPlot, score=TRUE, quant.scale=FALSE,
                   xlab = "Half-Normal Quantiles", ylab = "Residuals",
                   main = "Half-Normal plot",...){
  x <- residuals <- upper <- lower <- NULL
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
    theme_light()

  if(quant.scale==TRUE) {
    p <- p +
      scale_x_continuous(limits=c(0,1)) +
      scale_y_continuous(limits=c(0,1)) +
      coord_fixed(ratio = 1)
  }
  if(score==TRUE) {
    envScore <- calculateScorePDF(hnpObject)
    p <- p + geom_text(x = -Inf, y = Inf, label = paste("Score:",round(envScore,2)), hjust = -1, vjust = 1)

  }
  return(p)
}



# Creating dataset for Half-Normal Plot
datasetHalfNormalPlot <- function(hnpObject, quant.scale, ...){

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

# Calculating Likelihood for each residual
calculateKDE <- function(res, simres){
  simres <- as.numeric(simres)
  (length(simres)/2 - abs(sum(res<=simres) - length(simres)/2))/(length(simres)/2)
}


# Calculating PDF score
calculateScorePDF <- function(hnpObject){
  res <- hnpObject$residuals
  simres <- as.data.frame(t(hnpObject$all.sim))
  n <- length(res)
  PDFs <- mapply(calculateKDE, res, simres)
  return(sum(PDFs))
}

