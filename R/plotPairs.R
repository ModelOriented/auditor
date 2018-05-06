#' @title Model Correlation Plot
#'
#' @description Matrix of plots
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#' @param values "fitted" for model fitted values or "residuals" for residuals values
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom GGally ggpairs
#'
#' @export


plotPairs <- function(object, ..., values = "fitted"){

  if(values == "fitted") {
    df <- cbind(data.frame(y = object$y), getPairsDF(object, values))
  } else {
    df <- getPairsDF(object, values)
  }



  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- cbind( df, getPairsDF(resp, values) )
      }
    }
  }

  ggpairs(df) +
    theme_light() +
    ggtitle("Model Correlation")

}


getPairsDF <- function(object, values){
  if (values == "fitted") {
    df <- data.frame(values = object$fitted.values)
  }
  if (values == "residuals") {
    df <- data.frame(values = object$residuals)
  }

  colnames(df)[1] <- object$label
  return(df)
}



