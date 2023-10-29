#' @title Scale Location Plot in D3 with r2d3 package.
#'
#' @description
#' Function \code{plotD3_scalelocation} plots square root of the absolute value of the residuals vs target,
#' observed or variable values in the model. A vertical line corresponds to median.
#'
#' @param object An object of class \code{auditor_model_residual} created with \code{\link{model_residual}} function.
#' @param ... Other \code{auditor_model_residual} objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' If \code{variable = "_y_hat_"} the data on the plot will be ordered by predicted response.
#' If \code{variable = NULL}, unordered observations are presented.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's \code{FALSE}.
#' @param peaks Logical, indicates whenever peak observations should be highlighted. By default it's \code{FALSE}.
#' @param point_count Number of points to be plotted per model. Points will be chosen randomly. By default plot all of them.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's \code{TRUE}.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's \code{FALSE}.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever background plots should be plotted. By default it's FALSE.
#'
#' @return a \code{r2d3} object
#'
#' @seealso \code{\link{plot_scalelocation}}
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
#' plotD3_scalelocation(mr_lm, peaks = TRUE)
#'
#' @export
#' @rdname plotD3_scalelocation

plotD3_scalelocation <- function(object, ..., variable = NULL, smooth = FALSE,
                                peaks = FALSE, point_count = NULL, single_plot = TRUE,
                                scale_plot = FALSE, background = FALSE){

  n <- length(list(...)) + 1

  check_object(object, type = "res")

  df <- make_dataframe(object, ..., variable = variable, type = "scal")

  chart_title <- "Scale location"
  y_title <- "Sqrt|standarized residuals|"

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
  df <- df[, c("_sqrt_std_residuals_","_val_","_peak_","_label_")]
  colnames(df) <- c("y","x","peak","label")

  mrl <- split(df, f = df$label)

  model_names <- unlist(lapply(mrl, function(x) unique(x$label)))
  point_max <- point_min <- smooth_max <- smooth_min <- NULL
  point_data <- smooth_data <- NA

  # prepare points data

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
                  points = TRUE, smooth = smooth, abline = FALSE,
                  peaks = peaks, nlabel = FALSE,
                  scalePlot = scale_plot,
                  yTitle = y_title, chartTitle = chart_title)

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotScatterSingle.js", package = "auditor"),
               dependencies = list(
                 system.file("d3js/colorsDrWhy.js", package = "auditor"),
                 system.file("d3js/hackHead.js", package = "auditor")
               ),
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



#' @rdname plotD3_scalelocation
#' @export
plotD3ScaleLocation <- function(object, ..., variable = NULL, smooth = FALSE,
                                peaks = FALSE, point_count = NULL, single_plot = TRUE,
                                scale_plot = FALSE, background = FALSE){

  warning("Please note that 'plotD3ScaleLocation()' is now deprecated, it is better to use 'plotD3_scalelocation()' instead.")
  plotD3_scalelocation(object, ..., variable, smooth,
                                  peaks, point_count, single_plot,
                                  scale_plot, background)
}


