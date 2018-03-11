#' @title Receiver Operating Characteristic (ROC)
#'
#' @description Receiver Operating Characterstic Curve is a plot of the true positive rate (TPR) against the false positive rate (FPR) for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}, \link{plotRROC}, \link{plotREC}}
#'
#' @import ggplot2
#' @import plotROC
#'
#' @examples
#' library(auditor)
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#'
#' model.glm <- glm(diabetes~., family=binomial,	data=PimaIndiansDiabetes)
#' au.glm <- audit(model.glm, label="class glm")
#' plotROC(au.glm)
#'
#' model.glm.press <- glm(diabetes~pressure, family=binomial,	data=PimaIndiansDiabetes)
#' au.glm.press <- audit(model.glm.press)
#' plotROC(au.glm, au.glm.press)
#'
#' @export


plotROC <- function(object, ...){
  D <- m <- label <- NULL
  df <- getROCDF(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getROCDF(resp) )
      }
    }
  }

  ggplot(df, aes(d = D, m = m, color = label)) +
    geom_roc() +
    theme_light()
}

getROCDF <- function(object){
 return(data.frame(D=object$y, m = object$fitted.values, label=object$label))
}
