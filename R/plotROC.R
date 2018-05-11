#' @title Receiver Operating Characteristic (ROC)
#'
#' @description Receiver Operating Characterstic Curve is a plot of the true positive rate (TPR) against the false positive rate (FPR) for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}, \link{plotRROC}, \link{plotREC}}
#'
#' @import ggplot2
#' @import plotROC
#'
#' @examples
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#' glm_model <- glm(diabetes~., family=binomial,	data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes)
#' plotROC(glm_au)
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
    xlab("false positive fraction") +
    ylab("true positive fraction") +
    ggtitle("ROC Curve") +
    theme_light()
}

getROCDF <- function(object){
 return(data.frame(D=object$y, m = object$fitted.values, label=object$label))
}
