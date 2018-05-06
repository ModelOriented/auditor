#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#' @param variable name of dependent or independent variable to order residuals. If NULL the order from data is taken. If "Fitted values" then data is ordered by fitted values.
#' @param alpha confidence level of the interval
#'
#' @import ggplot2
#' @importFrom forecast Acf
#'
#' @export
plotACF <- function(object, ..., variable=NULL, alpha = 0.95){
  lag <- acf <- ymin <- NULL

  df <- getOrderedResiduals(object, variable)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getOrderedResiduals(resp, variable) )
      }
    }
  }

  conf_lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(nrow(object$data))

  ggplot(df, aes(x = lag)) +
    geom_segment(aes(x=lag, xend=lag, y=ymin, yend=acf)) +
    geom_hline(yintercept=conf_lims[1], color='blue', linetype = "dashed") +
    geom_hline(yintercept=conf_lims[2], color='blue', linetype = "dashed") +
    facet_grid(label ~ ., switch = "y") +
    theme_light() +
    ggtitle("ACF plot") +
    ylab("")

}

getOrderedResiduals <- function(object, variable){

  if(is.null(variable)){
    values <- seq(1, nrow(object$data))
  } else {
    if(variable == "Fitted values") {
      values <- object$fitted.values
    } else {
      values <- object$data[,variable]
    }
  }

  tmpDF <- data.frame(values = values, residuals=object$residuals)
  orderedResiduals <- dplyr::arrange(tmpDF, values)$residuals
  acf <- Acf(orderedResiduals, plot = FALSE, calc.ci = TRUE)

  resultDF <- data.frame(acf = acf$acf[-1], label = object$label, lag = acf$lag[-1], ymin = 0)

  return(resultDF)
}
