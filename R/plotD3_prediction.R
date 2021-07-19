#' @title Plot Prediction vs Target, Observed or Variable Values in D3 with r2d3 package.
#'
#' @description
#' Function \code{plotD3_prediction} plots predicted values observed or variable values in the model.
#'
#' @param object An object of class 'auditor_model_residual.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' If \code{variable = "_y_hat_"} the data on the plot will be ordered by predicted response.
#' If \code{variable = NULL}, unordered observations are presented.
#' @param points Logical, indicates whenever observations should be added as points. By default it's TRUE.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param abline Logical, indicates whenever function y = x should be added. Works only
#' with \code{variable = NULL} which is a default option.
#' @param point_count Number of points to be plotted per model. Points will be chosen randomly. By default plot all of them.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's TRUE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever background plots should be plotted.
#' By default it's FALSE.
#'
#' @return a \code{r2d3} object
#'
#' @seealso \code{\link{plot_prediction}}
#'
#' @examples
#'
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
#' plotD3_prediction(mr_lm, abline = TRUE)
#' plotD3_prediction(mr_lm, variable = "height", smooth = TRUE)
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(rf_audit)
#' plotD3_prediction(mr_lm, mr_rf, variable = "weight", smooth = TRUE)
#'
#' @export
#' @rdname plotD3_prediction
plotD3_prediction <- function(object, ..., variable = '_y_', points = TRUE, smooth = FALSE,
                              abline = FALSE, point_count = NULL, single_plot = TRUE,
                              scale_plot = FALSE, background = FALSE){

  if (points == FALSE & smooth == FALSE) stop("Plot points or smooth.")

  n <- length(list(...)) + 1

  check_object(object, type = "res")

  df <- make_dataframe(object, ..., variable = variable, type = "res")

  chart_title <- "Predicted"
  y_title <- "Predicted values"

  # set value for label of the X axis
  if (is.null(variable)) {
    x_title <- "Observations"
  } else if (variable == "_y_")  {
    x_title <- "Target variable"
  } else if (variable == "_y_hat_") {
    x_title <- "Actual response"
    chart_title <- paste0(chart_title, " vs ", x_title)
  } else {
    x_title <- as.character(df$`_variable_`[1])
    chart_title <- paste0(chart_title, " vs ", x_title)
  }

  # take only columns needed
  df <- df[, c('_y_hat_',"_val_","_label_")]
  colnames(df) <- c("y","x","label")

  mrl <- split(df, f = df$label)

  model_names <- unlist(lapply(mrl, function(x) unique(x$label)))
  point_max <- point_min <- smooth_max <- smooth_min <- NULL
  point_data <- smooth_data <- NA

  # prepare points data
  if (points == TRUE) {

    # find instance count and adjust point_count
    m <- dim(mrl[[1]])[1]
    if (is.null(point_count) || point_count > m) {
      point_data <- mrl
    } else {
      point_data <- lapply(mrl, function(mr) {
        mr <- mr[sample(m, point_count),]
        mr
      })
    }

    names(point_data) <- model_names
    point_max <- max(sapply(mrl, function(x) max(x$y)))
    point_min <- min(sapply(mrl, function(x) min(x$y)))
  }

  # prepare smooth data
  if (smooth == TRUE) {

    smooth_data <- lapply(mrl, function(mr) {
      model <- mgcv::gam(y ~ s(x, bs = "cs"), data = mr)
      vec <- data.frame(x = seq(min(mr$x), max(mr$x), length.out = 100))
      p <- predict(model, vec)
      df <- data.frame(x = vec$x, smooth = as.numeric(p))
      dim(df$x) <- NULL
      df
    })

    names(smooth_data) <- model_names
    smooth_max <- max(sapply(smooth_data, function(x) max(x$smooth)))
    smooth_min <- min(sapply(smooth_data, function(x) min(x$smooth)))
  }

  # find x and y scale
  xmax <- max(mrl[[1]]$x)
  xmin <- min(mrl[[1]]$x)
  ymax <- max(point_max, smooth_max)
  ymin <- min(point_min, smooth_min)

  ticks_margin <- abs(ymin-ymax)*0.15;

  temp <- jsonlite::toJSON(list(point_data, smooth_data))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax + ticks_margin, ymin = ymin - ticks_margin,
                  xTitle = x_title, n = n,
                  points = points, smooth = smooth, abline = abline,
                  peaks = FALSE, nlabel = FALSE,
                  scalePlot = scale_plot,
                  yTitle = y_title, chartTitle = chart_title)

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotScatterSingle.js", package = "auditor"),
               dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)

  } else {
    if (n==1) stop("Use single_plot instead.")
    options['background'] <- background

    r2d3::r2d3(data = temp, script = system.file("d3js/plotScatterMany.js", package = "auditor"),
               dependencies = list(
                 system.file("d3js/colorsDrWhy.js", package = "auditor"),
                 system.file("d3js/hackHead.js", package = "auditor")
               ),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)
  }
}

#' @rdname plotD3_prediction
#' @export
plotD3Prediction <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                             abline = FALSE,
                             point_count = NULL, single_plot = TRUE, scale_plot = FALSE,
                             background = FALSE) {
  warning("Please note that 'plotD3Prediction()' is now deprecated, it is better to use 'plotD3_prediction()' instead.")
  plotD3_prediction(object, ..., variable, points, smooth,
           point_count, single_plot, scale_plot,
           background)
}
