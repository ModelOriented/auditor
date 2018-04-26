#' @title Heatmap for shapley values
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
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
