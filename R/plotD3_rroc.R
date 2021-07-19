#' @title Regression Receiver Operating Characteristic (RROC) in D3 with r2d3 package.
#'
#' @description The basic idea of the ROC curves for regression is to show model asymmetry.
#' The RROC is a plot where on the x-axis we depict total over-estimation and on the y-axis total
#' under-estimation.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object
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
#' @seealso \code{ \link{plotD3_rroc}}
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # use DALEX package to wrap up a model into explainer
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' mr_lm <- model_residual(lm_audit)
#'
#' # plot results
#' plotD3_rroc(mr_lm)
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(rf_audit)
#' plotD3_rroc(mr_lm, mr_rf)
#'
#' @export
plotD3_rroc <- function(object, ..., scale_plot = FALSE) {

  x_title <- "Over-estimation"
  y_title <- "Under-estimation"
  chart_title <- "RROC Curve"

  # check if passed object is of class "auditor_model_residual"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "rroc")
  colnames(df) <- c("rroc_x", "rroc_y", "label", "curve")
  df <- df[is.finite(df$rroc_x) & is.finite(df$rroc_y),]

  n_models  <- length(unique(df$label))
  xmax <- max(df$rroc_x)
  xmin <- min(df$rroc_x)
  ymax <- max(df$rroc_y)
  ymin <- min(df$rroc_y)

  line_data <- split(df, f = df$label)
  line_data <- lapply(line_data, function(x) {
    x[order(x$rroc_x),]
  })

  temp <- jsonlite::toJSON(list(line_data))

  options <- list(xmin = xmin, xmax = xmax,
                  ymin = ymin, ymax = ymax,
                  scalePlot = scale_plot, n = n_models,
                  xTitle = x_title, yTitle = y_title,
                  chartTitle = chart_title)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotRROC.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/d3-tip.js", package = "auditor"),
               system.file("d3js/hackHead.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}

