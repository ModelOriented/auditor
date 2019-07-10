#' @title Regression Error Characteristic Curves (REC)
#'
#' @description Error Characteristic curves are a generalization of ROC curves.
#' On the x axis of the plot there is an error tolerance and on the y axis there is a percentage
#' of observations predicted within the given tolerance.
#'
#' @param object An object of class ModelAudit or modelResiduals.
#' @param ... Other modelAudit or model Residuals objects to be plotted together.
#'
#' @return ggplot object
#'
#' @details REC curve estimates the Cumulative Distribution Function (CDF) of the error
#'
#' Area Over the REC Curve (REC) is a biased estimate of the expected error
#'
#' @references Bi J., Bennett K.P. (2003). Regression error characteristic curves, in: Twentieth
#' International Conference on Machine Learning (ICML-2003), Washington, DC.
#'
#' @import ggplot2
#'
#' @seealso \code{\link{plot.modelAudit}, \link{plotROC}, \link{plotRROC}}
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotREC(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plotREC(lm_au, rf_au)
#'
#'
#' @export
plotREC <- function(object, ...) {

  # some safeguard
  rec_x <- rec_y <- label <- ord <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "rec")

  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(df$label)), df$label)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main chart
  p <- ggplot(data = df, aes(x = rec_x, y = rec_y)) +
    geom_line(aes(colour = label, group = ord))

  # theme, colours, titles, axes, scales, etc.
  p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, max(df$rec_x) * 1.1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, max(df$rec_y) * 1.1), labels = scales::percent) +
    xlab("Error tolerance") +
    ylab("") +
    ggtitle("REC Curve")

}
