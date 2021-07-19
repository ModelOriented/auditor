#' @title Influence of observations Plot in D3 with r2d3 package.
#'
#' @description Plot of Cook’s distances used for estimate the influence of an single observation.
#'
#' @param object An object of class 'auditor_model_cooksdistance' created with \code{\link{model_cooksdistance}} function.
#' @param ... Other objects of class 'auditor_model_cooksdistance'.
#' @param nlabel Number of observations with the biggest Cook's distances to be labeled.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's FALSE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever background plots should be plotted. By default it's FALSE.
#'
#' @details Cook’s distance is a tool for identifying observations that may negatively affect the model.
#' They may be also used for indicating regions of the design space where it would be good to obtain more observations.
#' Data points indicated by Cook’s distances are worth checking for validity.
#'
#' Cook’s Distances are calculated by removing the i-th observation from the data and recalculating the model.
#' It shows how much all the values in the model change when the i-th observation is removed.
#'
#' For model classes other than lm and glm the distances are computed directly from the definition.
#'
#' @references Cook, R. Dennis (1977). "Detection of Influential Observations in Linear Regression". doi:10.2307/1268249.
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
#' cd_lm <- model_cooksdistance(lm_audit)
#'
#' # plot results
#' plotD3_cooksdistance(cd_lm, nlabel = 5)
#'
#' @seealso \code{\link{plot_cooksdistance}}
#'
#' @export
#' @rdname plotD3_cooksdistance
plotD3_cooksdistance <- function(object, ..., nlabel = 3,
                              single_plot = FALSE, scale_plot = FALSE, background = FALSE){

  n <- length(list(...)) + 1

  x_title <- "Observation index"
  y_title <- "Cook's distance"
  chart_title <- "Influence of observations"

  check_object(object, type = "infl")

  df <- make_dataframe(object, ..., variable = NULL, type = "infl", nlabel = nlabel)
  colnames(df) <- c("y", "label", "x", "big")

  oil <- split(df, f = df$label)

  model_names <- unlist(lapply(oil, function(x) unique(x$label)))
  ymax <- ymin <- NULL

  # prepare points data
  point_data <- oil

  names(point_data) <- model_names
  ymax <- max(sapply(oil, function(x) max(x$y)))
  ymin <- min(sapply(oil, function(x) min(x$y)))

  # find x and y scale
  xmax <- max(oil[[1]]$x)
  xmin <- min(oil[[1]]$x)

  ticks_margin <- abs(ymin-ymax)*0.15;

  temp <- jsonlite::toJSON(list(point_data))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax + ticks_margin, ymin = ymin - ticks_margin,
                  xTitle = x_title, yTitle = y_title, n = n,
                  scalePlot = scale_plot, chartTitle = chart_title)

  if (n==1) single_plot = TRUE

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotCooksDistanceSingle.js", package = "auditor"),
               dependencies = list(
                 system.file("d3js/colorsDrWhy.js", package = "auditor"),
                 system.file("d3js/hackHead.js", package = "auditor")
               ),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)

  } else {
    options['background'] <- background

    r2d3::r2d3(data = temp, script = system.file("d3js/plotCooksDistanceMany.js", package = "auditor"),
               dependencies = list(
                 system.file("d3js/colorsDrWhy.js", package = "auditor"),
                 system.file("d3js/hackHead.js", package = "auditor")
               ),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)
  }
}

#' @rdname plotD3_cooksdistance
#' @export
plotD3CooksDistance <- function(object, ..., nlabel = 3,
                    single_plot = FALSE, scale_plot = FALSE, background = FALSE){
  warning("Please note that 'plotD3CooksDistance()' is now deprecated, it is better to use 'plotD3_cooksdistance()' instead.")
  plotD3_cooksdistance(object, ..., nlabel,
           single_plot, scale_plot, background)
}
