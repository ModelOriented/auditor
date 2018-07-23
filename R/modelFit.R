#' @title Create Model Fit explainer
#'
#' @description  Creates modelFit object to be plotted.
#'
#' @param object An object of class ModelAudit.
#' @param quant.scale if TRUE values on axis are on quantile scale.
#' @param ... other parameters passed do \code{\link[hnp]{hnp}} function.
#'
#' @export
modelFit <- function(object, quant.scale = FALSE, ...){
  if(!("modelAudit" %in% class(object))) stop("The function requires an object created with audit().")

  data <- NULL

    model <- object$model
    data <- object$data

  if("randomForest" %in% class(model)) {
    hnpObject <- hnpObject.randomForest(object, data)
  } else {
    hnpObject <- hnp(model, plot.sim=FALSE, ...)
  }

  result <- datasetHalfNormalPlot(hnpObject, quant.scale, ...)


  class(result) <- c("modelFit", "data.frame")

  return(result)
}

hnpObject.randomForest <- function(object, data){
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
    mod <- update(object$model, data = newdata)
    return(mod)
  }
  hnpObject <- hnp(object$model, newclass = TRUE, diagfun = d.fun, simfun = s.fun, fitfun = f.fun)

  return(hnpObject)
}

datasetHalfNormalPlot <- function(hnpObject, quant.scale, ...){

  n <- length(hnpObject$residuals)

  if (quant.scale == FALSE) {
    dataPlot <- data.frame(x = hnpObject$x, lower = hnpObject$lower,
                           median = hnpObject$median, upper = hnpObject$upper,
                           residuals = hnpObject$residuals,
                           all.sim = hnpObject$all.sim)
  } else {
    quantilesResiduals <-  seq(0,1,length.out = n)
    quantilesTheoretical <- phalfnorm(hnpObject$residuals)
    invQuantile <- ecdf(hnpObject$residuals)
    quantilesUpper <- invQuantile(hnpObject$upper)
    quantilesMedian <- invQuantile(hnpObject$median)
    quantilesLower <- invQuantile(hnpObject$lower)
    all.sim <- invQuantile(hnpObject$all.sim)
    dataPlot <- data.frame(x = quantilesTheoretical, lower = quantilesLower,
                           median = quantilesMedian, upper = quantilesUpper,
                           residuals = quantilesResiduals,
                           all.sim = all.sim)
  }
  return(dataPlot)
}

