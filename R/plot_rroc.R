#' @title Regression Receiver Operating Characteristic (RROC)
#'
#' @description The basic idea of the ROC curves for regression is to show model asymmetry.
#' The RROC is a plot where on the x-axis we depict total over-estimation and on the y-axis total
#' under-estimation.
#'
#' @param object AAn object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#'
#' @return A ggplot object.
#'
#' @details For RROC curves we use a shift, which is an equivalent to the threshold for ROC curves.
#' For each observation we calculate new prediction: \eqn{\hat{y}'=\hat{y}+s} where s is the shift.
#' Therefore, there are different error values for each shift: \eqn{e_i = \hat{y_i}' - y_i}
#'
#' Over-estimation is calculated as: \eqn{OVER= \sum(e_i|e_i>0)}.
#'
#' Under-estimation is calculated as: \eqn{UNDER = \sum(e_i|e_i<0)}.
#'
#'  The shift equals 0 is represented by a dot.
#'
#'  The Area Over the RROC Curve (AOC) equals to the variance of the errors multiplied by \eqn{frac{n^2}{2}}.
#'
#' @references Hernández-Orallo, José. 2013. ‘ROC Curves for Regression’. Pattern Recognition 46 (12): 3395–3411.
#'
#' @seealso \code{ \link{plot_roc}, \link{plot_rec}}
#'
#' @return A ggplot object.
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
#'
#' # plot results
#' plot_rroc(mr_lm)
#' plot(mr_lm, type = "rroc")
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(exp_rf)
#' plot_rroc(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf, type="rroc")
#'
#' @import ggplot2
#'
#' @export
plot_rroc <- function(object, ...) {

  # some safeguard
  rroc_x <- rroc_y <- label <- curve <- ord <- NULL

  # check if passed object is of class "auditor_model_residual"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "rroc")

  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(df$label)), df$`_label_`)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$`_label_`))))
  # main chart
  p <- ggplot(data = df, aes(x = rroc_x, y = rroc_y, colour = label)) +
    geom_line(data = subset(df, curve == TRUE), aes(group = ord)) +
    geom_point(data = subset(df, curve == FALSE), aes(colour = label), size = 2, show.legend = FALSE)

  # theme, colours, titles, axes, scales, etc.
  p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"),
          plot.title = element_text(margin = margin(b = 10)),
          legend.margin = margin(b = 15)) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, max(df[df$rroc_x !=  Inf, ]$rroc_x) * 1.1), breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0), limits = c(min(df[df$rroc_y != -Inf, ]$rroc_y) * 1.1, 0), breaks = scales::pretty_breaks()) +
    ylab("Under-estimation") +
    xlab("Over-estimation") +
    ggtitle("RROC Curve")


}

#' @rdname plot_rroc
#' @export
plotRROC <- function(object, ...) {
  message("Please note that 'plotRROC()' is now deprecated, it is better to use 'plot_rroc()' instead.")
  plot_rroc(object, ...)
}
