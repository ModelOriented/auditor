#' @title Predicted response vs Observed or Variable Values
#'
#' @description Plot of predicted response vs observed or variable Values.
#'
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit objects. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param smooth Logical, indicates whenever smooth line should be added.
#' @param abline Logical, indicates whenever function y=x shoulbe added.
#' @param split Character. If "model" plot will be splitted by model.
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotPrediction(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotPrediction(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotPrediction <- function(object, ..., variable = NULL, smooth = FALSE, abline = TRUE, split = "none"){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if("modelResiduals" %in% class(object)) variable <- object$variable[1]
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object, variable)
  val <- fitted.values <- label <- NULL

  df <- object

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) df <- rbind( df, modelResiduals(resp, variable) )
      if("modelResiduals" %in% class(resp)) df <- rbind(df, resp)
    }
  }

  if(is.null(variable) || is.na(variable)) variable <- "Index"

  maybeVS <- ifelse(variable == "Index", "", paste("vs",variable))
  maybeAbline <- NULL
  if(abline == TRUE) maybeAbline <- geom_abline()
  maybeSplit <- NULL
  maybeLegendBottom <- NULL
  if(split == "model") {
    maybeSplit <- facet_wrap(label~.)
    maybeLegendBottom <- theme(legend.position = "bottom")
  }

  p <- ggplot(df, aes(val, fitted.values, color = label)) +
          geom_point() +
          maybeAbline +
          maybeSplit +
          xlab(variable) +
          ylab("Predicted values") +
          ggtitle(paste("Predicted", maybeVS)) +
          theme_light() +
          maybeLegendBottom

  if(smooth == TRUE){
    p <- p + geom_smooth(method = "loess", se = FALSE)
  }

  if(variable == "Observed response") p <- p + geom_abline(slope = 1, intercept = 0)

  return(p)
}


