#' @title Model Correlation Plot
#'
#' @description Matrix of plots
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
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
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if("modelResiduals" %in% class(object)) variable <- object$variable[1]
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object)
  x <- y <- NULL

  if((values == "Fitted values") || (values == "Predicted response")) {
    df <- data.frame(y = object$y, fit = object$fitted.values)
    colnames(df)[2] <- as.character(object$label[1])
  } else {
    df <- data.frame(y = object$res)
    colnames(df)[1] <- as.character(object$label[1])
  }


  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) resp <-  modelResiduals(resp)
      if((values == "Fitted values") || (values == "Predicted response")) {
        df_tmp <- data.frame(resp$fitted.values)
      } else {
        df_tmp <- data.frame(resp$res)
      }
      colnames(df_tmp)[1] <- as.character(resp$label[1])
      df <- cbind(df, df_tmp)
    }
  }

  ggpairs(df) +
    theme_light() +
    ggtitle("Model Correlation")

}
