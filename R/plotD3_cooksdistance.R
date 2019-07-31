#' @title Influence of observations Plot in D3 with r2d3 package.
#'
#' @description Cook’s distances are used for estimate the influence of an single observation.
#' If the picture is not displayed in the viewer, please update your RStudio.
#'
#' @param object An object of class 'model_audit' or 'model_cooksdistance'.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param nlabel Number of observations with the biggest Cook's distances to be labeled.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's FALSE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever backgroud plots should be plotted. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#'
#' @seealso \code{\link{plot_cooksdistance}}
#'
#' @export
#' @rdname plotD3_cooksdistance

plotD3_cooksdistance <- function(object, ..., nlabel = 3,
                              single_plot = FALSE, scale_plot = FALSE, background = FALSE){

  n <- length(list(...)) + 1

  aul <- list(object, ...)

  xTitle <- "Observations"
  yTitle <- "Cook's distance"
  chartTitle <- "Influence of observations"

  # make every input modelResiduals, check `variable`
  oil <- list()

  for (i in 1:n) {
    object <- aul[[i]]

    check_object(object, type = "infl")

    oi <- object

    oi$big <- c(rep(TRUE, nlabel), rep(FALSE, nrow(oi)-nlabel))
    colnames(oi) <- c("y", "label", "x", "big")

    oil[[i]] <- oi
  }

  modelNames <- unlist(lapply(oil, function(x) unique(x$label)))
  pointMax <- pointMin <- NULL

  # prepare points data
  pointData <- oil

  names(pointData) <- modelNames
  pointMax <- max(sapply(oil, function(x) max(x$y)))
  pointMin <- min(sapply(oil, function(x) min(x$y)))

  # find x and y scale
  xmax <- max(oil[[1]]$x)
  xmin <- min(oil[[1]]$x)

  ticksMargin <- abs(pointMin-pointMax)*0.15;

  temp <- jsonlite::toJSON(list(pointData))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = pointMax + ticksMargin, ymin = pointMin - ticksMargin,
                  xTitle = xTitle, yTitle = yTitle, nlabel = nlabel, n = n,
                  scalePlot = scale_plot, chartTitle = chartTitle)

  if (n==1) single_plot = TRUE

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotCooksDistanceSingle.js", package = "auditor"),
               dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)

  } else {
    options['background'] <- background

    r2d3::r2d3(data = temp, script = system.file("d3js/plotCooksDistanceMany.js", package = "auditor"),
               dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)
  }
}

#' @rdname plot_cooksdistance
#' @export
plotD3CooksDistance <- function(object, ..., nlabel = 3,
                    single_plot = FALSE, scale_plot = FALSE, background = FALSE){
  message("Please note that 'plotD3CooksDistance()' is now deprecated, it is better to use 'plotD3_cooksdistance()' instead.")
  plotD3_cooksdistance(object, ..., nlabel,
           single_plot, scale_plot, background)
}