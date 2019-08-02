#' @title Regression Error Characteristic Curves (REC)
#'
#' @description Error Characteristic curves are a generalization of ROC curves.
#' On the x axis of the plot there is an error tolerance and on the y axis there is a percentage
#' of observations predicted within the given tolerance.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#'
#' @return A ggplot object.
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
#' @seealso \code{\link{plotROC}, \link{plotRROC}}
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' lm_model <- lm(life_length ~ ., data = dragons)
#'
#' # use DALEX package to wrap up a model into explainer
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' library(auditor)
#' lm_mr <- model_residual(lm_exp)
#' plot_rec(lm_mr)
#' plot(lm_mr, type = "rec")
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_exp <- DALEX::explain(rf_model, data = dragons, y = dragons$life_length)
#' rf_mr <- model_residual(rf_exp)
#' plot_rec(lm_mr, rf_mr)
#' plot(lm_mr, rf_mr, type = "rec")
#'
#'
#' @export
plot_rec <- function(object, ...) {

  # some safeguard
  rec_x <- rec_y <- label <- ord <- NULL

  # check if passed object is of class "auditor_model_residual"
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


#' @rdname plot_rec
#' @export
plotREC <- function(object, ...) {
  message("Please note that 'plotREC()' is now deprecated, it is better to use 'plot_rec()' instead.")
  plot_rec(object, ...)
}
