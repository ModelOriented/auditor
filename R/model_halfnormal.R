#' @title Create Halfnormal Explanation
#'
#' @description  Creates \code{auditor_model_halfnormal} object that can be used for plotting halfnormal plot.
#'
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param quant if TRUE values on axis are on quantile scale.
#' @param ... other parameters passed do \code{\link[hnp]{hnp}} function.
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' mh <- model_halfnormal(glm_audit)
#' mh
#'
#' plot(mh)
#'
#' @references Moral, R., Hinde, J., & Demétrio, C. (2017). Half-Normal Plots and Overdispersed Models in R: The hnp Package.doi:http://dx.doi.org/10.18637/jss.v081.i10
#'
#' @return An object of the class \code{auditor_model_halfnormal}.
#' @importFrom hnp hnp
#' @importFrom stats pnorm
#'
#' @export
model_halfnormal <- function(object, quant = FALSE, ...){
  check_object(object, type = "exp")

  data <- NULL

  model <- object$model
  data <- object$data

  if("randomForest" %in% class(model)) {
    hnpObject <- hnpObject.randomForest(object, data)
  } else {
    hnpObject <- hnp(model, plot.sim=FALSE, halfnormal=FALSE, ...)
  }

  result <- dataset_halfnormal_plot(hnpObject, quant, ...)

  class(result) <- c("auditor_model_halfnormal", "data.frame")

  result$`_label_` <- factor(object$label)

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
  hnpObject <- hnp(object$model, newclass = TRUE, diagfun = d.fun, simfun = s.fun, fitfun = f.fun, plot.sim = FALSE)

  return(hnpObject)
}

dataset_halfnormal_plot <- function(hnpObject, quant, ...){

  n <- length(hnpObject$residuals)

  if (quant == FALSE) {
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
  colnames(dataPlot)[1:5] <- c( "_x_", "_lower_", "_median_", "_upper_",  "_residuals_")
  return(dataPlot)
}


phalfnorm <- function(residuals)
{
  theta <- sqrt(pi/2)
  #lower tail
  sd.norm <- sqrt(pi/2)/theta
  p <- ifelse(residuals < 0, 0, 2*pnorm(residuals, mean=0, sd=sd.norm)-1)

  return(p)
}







#' @rdname model_halfnormal
#' @export
modelFit <- function(object, quant = FALSE, ...){
  warning("Please note that 'modelfit()' is now deprecated, it is better to use 'model_halfnormal()' instead.")
  model_halfnormal(object, quant)
}

