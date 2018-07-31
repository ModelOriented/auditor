#' @title Autocorrelation Function Plot
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit object. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param alpha Confidence level of the interval.
#'
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotACF(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotACF(lm_au, rf_au)
#'
#'
#' @import ggplot2
#' @importFrom stats qnorm acf
#'
#' @export
plotACF <- function(object, ..., variable=NULL, alpha = 0.95){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if("modelResiduals" %in% class(object)) variable <- object$variable[1]
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)

  lag <- acf <- ymin <- NULL


  df <- object

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) df <- rbind( df, modelResiduals(resp, variable) )
      if("modelResiduals" %in% class(resp)) df <- rbind(df, resp)
    }
  }

  resultDF <- data.frame(acf = numeric(), label = character(), lag = numeric(), ymin = numeric())
  for(label in unique(df$label)){
    orderedResiduals <- df[which(df$label == label), "res"]
    acf <- acf(orderedResiduals, plot = FALSE)
    resultDF <- rbind(resultDF, data.frame(acf = acf$acf[-1], label = label, lag = acf$lag[-1], ymin = 0))
  }

  conf_lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(nrow(object))

  ggplot(resultDF, aes(x = lag)) +
    geom_segment(aes(x=lag, xend=lag, y=ymin, yend=acf, color = label)) +
    geom_hline(yintercept=conf_lims[1], color='blue', linetype = "dashed") +
    geom_hline(yintercept=conf_lims[2], color='blue', linetype = "dashed") +
    facet_grid(label ~ ., switch = "y") +
    theme_light() +
    ggtitle("ACF plot") +
    ylab("")

}
