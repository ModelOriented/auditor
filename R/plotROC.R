#' @title Receiver Operating Characteristic (ROC)
#'
#' @description Receiver Operating Characterstic Curve is a plot of the true positive rate (TPR) against the false positive rate (FPR) for the different thresholds.
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @import ggplot2
#' @import plotROC
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
