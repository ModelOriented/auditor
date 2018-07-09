#' @title Plot Boxplots of Residuals
#'
#' @description A plot of residuals.
#'
#' @param object An object of class modelAudit.
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
#' plotResidualBoxplot(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export
plotResidualBoxplot <- function(object, ...){
  if(!("modelPerformance" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelPerformance().")
  if(!("modelPerformance" %in% class(object))) object <- modelPerformance(object)

  res <- label <- NULL

  df <- object

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) df <- rbind( df, modelPerformance(resp) )
      if("modelPerformance" %in% class(resp)) df <- rbind(df, resp)
    }
  }


  ggplot(df, aes(label, abs(res), fill = label)) +
    geom_boxplot(coef = 1000) +
    stat_summary(fun.y=function(x){sqrt(mean(x^2))}, geom="point", shape=20, size=10, color="red", fill="red") +
    xlab("") +
    ylab("") +
    ggtitle("Boxplots of | residuals |", "Red dot stands for root mean square of residuals") +
    theme_light() +
    coord_flip()
}


