#' @title Regression Receiver Operating Characteristic (RROC)
#'
#' @description The basic idea of the ROC curves for regression is to show model asymmetry.
#' The RROC is a plot where on the x-axis we depict total over-estimation and on the y-axis total
#' under-estimation.
#'
#' @param object An object of class \code{auditor_model_residual} created with \code{\link{model_residual}} function.
#' @param ... Other \code{auditor_model_residual} objects to be plotted together.
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
#' @references Hernández-Orallo, José. 2013. "ROC Curves for Regression". Pattern Recognition 46 (12): 3395–3411.
#'
#' @seealso \code{ \link{plot_roc}, \link{plot_rec}}
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' mr_lm <- model_residual(lm_audit)
#'
#' # plot results
#' plot_rroc(mr_lm)
#' plot(mr_lm, type = "rroc")
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(rf_audit)
#' plot_rroc(mr_lm, mr_rf)
#' plot(mr_lm, mr_rf, type="rroc")
#'
#' @export
plot_rroc <- function(object, ...) {

  # some safeguard
  `_rroc_x_` <- `_rroc_y_` <- `_label_` <- `_curve_` <- ord <- NULL

  # check if passed object is of class "auditor_model_residual"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "rroc")

  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(df$label)), df$`_label_`)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(nlevels(df$`_label_`)))
  # main chart
  p <- ggplot(data = df, aes(x = `_rroc_x_`, y = `_rroc_y_`, colour = `_label_`)) +
    geom_line(data = subset(df, `_curve_` == TRUE), aes(group = ord)) +
    geom_point(data = subset(df, `_curve_` == FALSE), aes(colour = `_label_`), size = 2, show.legend = FALSE)

  # theme, colours, titles, axes, scales, etc.
  p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3"),
          plot.title = element_text(margin = margin(b = 10)),
          legend.margin = margin(b = 15)) +
    scale_color_manual(values = rev(colours), breaks = levels(df$`_label_`), guide = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0, 0), limits = c(0, max(df[df$`_rroc_x_` !=  Inf, ]$`_rroc_x_`) * 1.1), breaks = scales::pretty_breaks()) +
    scale_y_continuous(expand = c(0, 0), limits = c(min(df[df$`_rroc_y_` != -Inf, ]$`_rroc_y_`) * 1.1, 0), breaks = scales::pretty_breaks()) +
    ylab("Under-estimation") +
    xlab("Over-estimation") +
    ggtitle("RROC Curve")


}

#' @rdname plot_rroc
#' @export
plotRROC <- function(object, ...) {
  warning("Please note that 'plotRROC()' is now deprecated, it is better to use 'plot_rroc()' instead.")
  plot_rroc(object, ...)
}
