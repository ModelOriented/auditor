#' @title Model Correlation Plot
#'
#' @description Matrix of plots
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param values "Fitted values" or "Predicted response" for model fitted values or "Residuals" for residual values.
#'
#' @return ggplot object
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotModelCorrelation(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom GGally ggpairs
#'
#' @export


plotModelCorrelation <- function(object, ..., values = "Fitted values"){

  if((values == "Fitted values") || (values == "Predicted response")) {
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
  if ((values == "Fitted values") || (values == "Predicted response")) {
    df <- data.frame(values = object$fitted.values)
  }
  if (values == "Residuals") {
    df <- data.frame(values = object$residuals)
  }

  colnames(df)[1] <- object$label
  return(df)
}



