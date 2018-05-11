#' @title Plot Residuals vs Observed, Fitted or Variable Values
#'
#' @description A plot of residuals against fitted values, observed values or any variable.
#'
#' @param object An object of class modelAudit.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param ... Other modelAudit objects to be plotted together.
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotResidual(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotResidual(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotResidual <- function(object, ..., variable=NULL){
  residuals <- values <- label <- NULL

  df <- generateResidualsDF(object, variable)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, generateResidualsDF(resp, variable) )
      }
    }
  }

  maybe_points <- if (length(unique(df$label)) ==1) df else df[0, ]
  if (is.null(variable)) {
    title <- "Residuals"
  } else {
    title <- paste0("Residuals vs ", variable)
  }


  ggplot(df, aes(values, residuals)) +
    geom_point(data = maybe_points) +
    geom_smooth(aes(color = label), method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("residuals") +
    ggtitle(title) +
    theme_light()
}


generateResidualsDF <- function(object, variable){
  resultDF <- orderResidualsDF(object, variable, is.df = TRUE)
  resultDF$label <- object$label
  resultDF <- resultDF[order(resultDF$values),]
  return(resultDF)
}

