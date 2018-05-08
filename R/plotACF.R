#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param variable Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param alpha Confidence level of the interval.
#'
#' @import ggplot2
#' @importFrom stats qnorm acf
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

  orderedResiduals <- orderResidualsDF(object, variable)

  acf <- acf(orderedResiduals, plot = FALSE)

  resultDF <- data.frame(acf = acf$acf[-1], label = object$label, lag = acf$lag[-1], ymin = 0)

  return(resultDF)
}
