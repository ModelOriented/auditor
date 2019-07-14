#' @title Regression Error Characteristic Curves (REC) in D3 with r2d3 package.
#'
#' @description Error Characteristic curves are a generalization of ROC curves.
#' On the x axis of the plot there is an error tolerance and on the y axis there is a percentage
#' of observations predicted within the given tolerance.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or model Residuals objects to be plotted together.
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
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotD3REC(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plotD3REC(lm_au, rf_au)
#'
#' @export
#' @rdname plotD3REC

plotD3REC <- function(object, ..., scale_plot = FALSE) {

  # some safeguard
  rec_x <- rec_y <- label <- NULL

  xTitle <- "Error tolerance"
  chartTitle <- "REC Curve"

  n <- length(list(object, ...))

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., type = "rec")

  #:#
  xmax <- max(df$rec_x)
  xmin <- min(df$rec_x)
  modelNames <- unique(df$label)

  colnames(df) <- c("x", "y", "label")

  lineData <- split(df, f = df$label)

  temp <- jsonlite::toJSON(list(lineData))

  options <- list(xmax = xmax, xmin = xmin,
                  scalePlot = scale_plot, n = n,
                  xTitle = xTitle, chartTitle = chartTitle)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotREC.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}
