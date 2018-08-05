#' @title Scale location plot
#'
#' @description Variable values vs square root of the absolute value of the residuals.
#' A vertical line corresponds to median.
#'
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit objects to be plotted together.
#' @param variable Only for modelAudit object. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param score A logical value. If TRUE value of \link{scorePeak} will be added.
#' @param peaks A logical value. If TRUE peaks are marked on plot by black dots.
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotScaleLocation(lm_au)
#'
#'
#' @import ggplot2
#' @importFrom stats median
#'
#' @export
plotScaleLocation <- function(object, ..., variable=NULL, score=FALSE, peaks = TRUE){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if("modelResiduals" %in% class(object)) variable <- object$variable[1]
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)
  values <- sqrt.std.residuals <- peak <- label <- NULL

  df <- generateScaleLocationDF(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) df <- rbind( df, generateScaleLocationDF(modelResiduals(resp, variable)) )
      if("modelResiduals" %in% class(resp)) df <- rbind(df, generateScaleLocationDF(resp))
    }
  }

  maybe_facet <- NULL
  if(length(list(...)) > 0) maybe_facet <- facet_grid(label ~ ., switch = "y")
  maybe_peaks <- NULL
  if(peaks == TRUE) maybe_peaks <- geom_point(data = subset(df, peak == TRUE), color = "black")

  p <- ggplot(df, aes(x = values, y = sqrt.std.residuals)) +
    maybe_facet +
    geom_point(aes(color = label)) +
    maybe_peaks +
    geom_smooth(method = "loess", se = FALSE) +
    xlab(variable) +
    ylab("\u221A|Standarized residuals|") +
    ggtitle("Scale Location") +
    theme_light()

  if(score==TRUE){
    score <- scorePeak(object, variable)
    p <- p + geom_text(x = -Inf, y = Inf, label = paste("ScorePeak:", round(score$score,2)), hjust = -1, vjust = 1)
  }
  return(p)
}

generateScaleLocationDF <- function(object){
  resultDF <- data.frame(std.residuals=object$std.res, values = object$val)
  resultDF$sqrt.std.residuals <- sqrt(abs(resultDF$std.residuals))
  resultDF$label <- object$label[1]
  resultDF$peak <- (abs(object$std.res) >= cummax(abs(object$std.res)))
  return(resultDF)
}
