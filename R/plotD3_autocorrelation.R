#' @title Autocorrelation Plot in D3 with r2d3 package.
#'
#' @description Plot of i-th residual vs i+1-th residual.
#'
#' @param object An object of class 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... Other 'auditor_model_residual' objects to be plotted together.
#' @param variable Name of variable to order residuals on a plot.
#' If \code{variable="_y_"}, the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link[DALEX]{explain}} function).
#' @param points Logical, indicates whenever observations should be added as points. By default it's TRUE.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param point_count Number of points to be plotted per model. Points will be chosen randomly.
#' By default plot all of them.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's TRUE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever background plots should be plotted.
#' By default it's FALSE.
#'
#' @return a \code{r2d3} object
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
#' plotD3_autocorrelation(mr_lm)
#' plotD3_autocorrelation(mr_lm, smooth = TRUE)
#'
#' @export
#' @rdname plotD3_autocorrelation
plotD3_autocorrelation <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                                  point_count = NULL, single_plot = TRUE, scale_plot = FALSE,
                                  background = FALSE) {

  if (points == FALSE & smooth == FALSE) stop("Plot points or smooth.")

  n <- length(list(...)) + 1

  check_object(object, type = "res")

  df_temp <- make_dataframe(object, ..., variable = variable, type = "res")

  df <- data.frame(x_val = numeric(), y_val = numeric(), label = character())
  for (label in levels(df_temp$`_label_`)) {
    ord_res <- df_temp[which(df_temp$`_label_` == label), "_residuals_"]
    df <- rbind(df, data.frame(x = ord_res[-length(ord_res)],
                               y = ord_res[-1],
                               label = label))
  }

  chart_title <- "Autocorrelation"
  x_title <- "Residual i"
  y_title <- "Residual i+1"

  if (is.null(variable)) {
    chart_title <- "Autocorrelation of unordered residuals"
  } else if (variable == "_y_")  {
    chart_title <- "Autocorrelation of residuals ordered by predicted values"
  } else if (variable == "_y_hat_") {
    chart_title <- "Autocorrelation of residuals ordered by model response"
  } else {
    chart_title <- paste0("Autocorrelation of residuals ordered by ",variable)
  }

  mrl <- split(df, f = df$label)

  model_names <- unlist(lapply(mrl, function(x) unique(x$label)))
  x_min <- x_max <- y_min <- y_max <- NULL
  point_data <- smooth_data <- NA

  # prepare points data
  if (points == TRUE) {

    # find instance count and adjust point_count
    m <- dim(mrl[[1]])[1]
    if (is.null(point_count) || point_count > m) {
      point_data <- mrl
    } else {
      point_data <- lapply(mrl, function(mr) {
        mr <- mr[sample(m,point_count),]
        mr
      })
    }

    names(point_data) <- model_names
  }

  # prepare smooth data
  if (smooth == TRUE) {

    smooth_data <- lapply(mrl, function(mr) {
      model <- mgcv::gam(y ~ s(x, bs = "cs"), data = mr)
      vec <- data.frame(x = seq(min(mr$x), max(mr$x), length.out = 100))
      p <- predict(model, vec)
      df <- data.frame(val = vec$x, smooth = as.numeric(p))
      dim(df$val) <- NULL
      df
    })

    names(smooth_data) <- model_names
  }

  xmax <- max(sapply(mrl, function(x) max(x$x, x$y)))
  xmin <- min(sapply(mrl, function(x) min(x$x, x$y)))
  ymax <- ifelse(all(is.na(smooth_data)), xmax,
                 max(sapply(smooth_data, function(x) max(x$smooth)), xmax))
  ymin <- ifelse(all(is.na(smooth_data)), xmin,
                 min(sapply(smooth_data, function(x) min(x$smooth)), xmin))

  temp <- jsonlite::toJSON(list(point_data, smooth_data))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax, ymin = ymin,
                  xTitle = x_title, n = n,
                  points = points, smooth = smooth,
                  scalePlot = scale_plot, yTitle = y_title,
                  chartTitle = chart_title)

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotAutocorrelationSingle.js", package = "auditor"),
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

    r2d3::r2d3(data = temp, script = system.file("d3js/plotAutocorrelationMany.js", package = "auditor"),
               dependencies = list(
                 system.file("d3js/colorsDrWhy.js", package = "auditor"),
                 system.file("d3js/hackHead.js", package = "auditor")
               ),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)
  }
}

#' @rdname plotD3_autocorrelation
#' @export
plotD3Autocorrelation <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                                  point_count = NULL, single_plot = TRUE, scale_plot = FALSE,
                                  background = FALSE) {
  warning("Please note that 'plotD3Autocorrelation()' is now deprecated, it is better to use 'plotD3_autocorrelation()' instead.")
  plotD3_autocorrelation(object, ..., variable, points, smooth,
           point_count, single_plot, scale_plot,
           background)
}
