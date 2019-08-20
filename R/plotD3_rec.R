#' @title Regression Error Characteristic Curves (REC) in D3 with r2d3 package.
#'
#' @description Error Characteristic curves are a generalization of ROC curves.
#' On the x axis of the plot there is an error tolerance and on the y axis there is a percentage
#' of observations predicted within the given tolerance.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#' @details REC curve estimates the Cumulative Distribution Function (CDF) of the error
#'
#' Area Over the REC Curve (REC) is a biased estimate of the expected error
#'
#' @references Bi J., Bennett K.P. (2003). Regression error characteristic curves, in: Twentieth
#' International Conference on Machine Learning (ICML-2003), Washington, DC.
#'
#' @seealso \code{\link{plot_rec}}
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
#' plotD3_rec(mr_lm)
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(exp_rf)
#' plotD3_rec(mr_lm, mr_rf)
#'
#' @export
#' @rdname plotD3_rec
plotD3_rec <- function(object, ..., scale_plot = FALSE) {

  # some safeguard
  rec_x <- rec_y <- label <- NULL

  x_title <- "Error tolerance"
  chart_title <- "REC Curve"

  n <- length(list(object, ...))

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "rec")

  #:#
  xmax <- max(df$rec_x)
  xmin <- min(df$rec_x)
  model_names <- unique(df$label)

  colnames(df) <- c("x", "y", "label")

  line_data <- split(df, f = df$label)

  temp <- jsonlite::toJSON(list(line_data))

  options <- list(xmax = xmax, xmin = xmin,
                  scalePlot = scale_plot, n = n,
                  xTitle = x_title, chartTitle = chart_title)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotREC.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}


#' @rdname plotD3_rec
#' @export
plotD3REC <- function(object, ..., scale_plot = FALSE) {
  message("Please note that 'plotD3REC()' is now deprecated, it is better to use 'plotD3_rec()' instead.")
  plotD3_rec(object, ..., scale_plot)
}
