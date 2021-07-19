#' @title Plot Residuals vs Observed, Fitted or Variable Values in D3 with r2d3 package.
#'
#' @description
#' Function \code{plotD3_residual} plots residual values vs fitted, observed or variable values in the model.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' If \code{variable = "_y_hat_"} the data on the plot will be ordered by predicted response.
#' If \code{variable = NULL}, unordered observations are presented.
#' @param points Logical, indicates whenever observations should be added as points. By default it's TRUE.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param std_residuals Logical, indicates whenever standardized residuals should be used. By default it's FALSE.
#' @param nlabel Number of observations with the biggest residuals to be labeled.
#' @param point_count Number of points to be plotted per model. Points will be chosen randomly. By default plot all of them.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's TRUE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever background plots should be plotted.
#'  By default it's FALSE.
#'
#' @return a \code{r2d3} object
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
#' plotD3_residual(mr_lm)
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mr_rf <- model_residual(rf_audit)
#' plotD3_residual(mr_lm, mr_rf)
#'
#' @seealso \code{\link{plot_residual}}
#'
#' @export
#' @rdname plotD3_residual

plotD3_residual <- function(object, ..., variable = '_y_', points = TRUE, smooth = FALSE,
                           std_residuals = FALSE, nlabel = 0,
                           point_count = NULL, single_plot = TRUE,
                           scale_plot = FALSE, background = FALSE){

  if (points == FALSE & smooth == FALSE) stop("Plot points or smooth.")

  n <- length(list(...)) + 1

  check_object(object, type = "res")

  df <- make_dataframe(object, ..., variable = variable, type = "res")

  # chose y
  if (std_residuals == TRUE) {
    y <- "_std_residuals_"
    y_title <- "Standardized residuals"
    chart_title <- "Standardized residuals"
  } else {
    y <- "_residuals_"
    y_title <- "Residuals"
    chart_title <- "Residuals"
  }

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
  df <- df[, c(y,"_val_","_label_","_index_")]
  colnames(df) <- c("y","x","label", "index")

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

    if (nlabel > 0) {
      point_data <- lapply(point_data, function(x) {
        x <- x[order(abs(x$y), decreasing = TRUE), ]
        x$big <- c(rep(TRUE, nlabel), rep(FALSE, dim(x)[1] - nlabel))
        x
      })
    }
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
                  points = points, smooth = smooth, abline = FALSE,
                  peaks = FALSE, nlabel = ifelse(nlabel>0, TRUE, FALSE),
                  scalePlot = scale_plot,
                  yTitle = y_title, chartTitle = chart_title)

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotScatterSingle.js", package = "auditor"),
               dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)

  } else {
    if (n == 1) stop("Use single_plot instead.")
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

#' @rdname plotD3_residual
#' @export
plotD3Residual <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                           std_residuals = FALSE,point_count = NULL, single_plot = TRUE,
                           scale_plot = FALSE, background = FALSE){
  warning("Please note that 'plotD3Residual()' is now deprecated, it is better to use 'plotD3_residual()' instead.")
  plotD3_residual(object, ..., variable, points, smooth,
                  std_residuals, point_count, single_plot,
                  scale_plot, background)
}


