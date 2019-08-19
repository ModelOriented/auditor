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
#' @seealso \code{\link{plot_roc}, \link{plot_rroc}}
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_lm <- DALEX::explain(model_lm, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' library(auditor)
#' mr_lm <- model_residual(exp_lm)
#' plot_rec(mr_lm)
#' plot(mr_lm, type = "rec")
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(exp_rf)
#' plot_rec(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf, type = "rec")
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
  colours <- rev(theme_drwhy_colors(length(levels(df$`_label_`))))

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
