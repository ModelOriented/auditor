#' @title Cumulative Gain Chart
#'
#' @description Cumulative Gain Chart is is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class modelAudit or modelEvaluation.
#' @param ... Other modelAudit objects to be plotted together.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @examples
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#' glm_model <- glm(diabetes~., family=binomial,	data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes)
#' plotCumulativeGain(glm_au)
#'
#' @import ggplot2
#' @importFrom ROCR performance prediction
#'
#'
#' @export


plotCumulativeGain <- function(object, ...){
  if(!("modelEvaluation" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelEvaluation().")
  if("modelAudit" %in% class(object)) object <- modelEvaluation(object)
  y <- fitted.values <- label <- NULL

  df <- object

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) resp <- modelEvaluation(resp)
      if("modelEvaluation" %in% class(resp))  df <- rbind( df, resp )
    }
  }

  for(lab in unique(df$label)) df <- rbind(df, c(0, 0, 0, 0, Inf, lab))
  df$tpr <- as.numeric(df$tpr)
  df$rpp <- as.numeric(df$rpp)
  df$alpha <- as.numeric(df$alpha)

  ggplot(df, aes(x = rpp, y = tpr, color = label)) +
    geom_line() +
    xlab("rate of positive prediction") +
    ylab("true positive rate") +
    ggtitle("Cumulative Gain") +
    theme_light()
}




