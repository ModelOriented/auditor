#' @title Plot Residuals vs Target, Observed or Variable Values in D3 with r2d3 package.
#'
#' @description
#' Function \code{plotD3_residual} plots resudial values vs target, observed or variable values in the model.
#' It uses output from \code{model_audit} or \code{model_residual} function.
#'
#' If the picture is not displayed in the viewer, please update your RStudio.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit objects. Name of model variable to order residuals.
#' If value is NULL the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{audit}} function). One can also pass any name of any other variable
#' in the data set. If \code{variable = ""} is set, unordered observations are presented.
#' @param points Logical, indicates whenever observations should be added as points. By defaul it's TRUE.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param std_residuals Logical, indicates whenever standardized residuals should be used. By default it's FALSE.
#' @param point_count Number of points to be plotted per model. Points will be chosen randomly. By default plot all of them.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's TRUE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever backgroud plots should be plotted. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#' @examples
#' library("auditor")
#' library("DALEX")
#'
#' lm_model <- lm(m2.price ~., data = apartments)
#' lm_au <- audit(lm_model, label = "lm")
#' plotD3Residual(lm_au, variable = "construction.year")
#'
#' library(randomForest)
#' rf_model <- randomForest(m2.price ~., data = apartments)
#' rf_au <- audit(rf_model, label = "rf")
#' rf_mr <- modelResiduals(rf_au, "construction.year")
#' plotD3_residual(lm_au, rf_mr, variable = "construction.year", smooth = TRUE)
#' plotD3_residual(lm_au, rf_mr, variable = "construction.year", smooth = TRUE, single_plot = FALSE)
#'
#' @seealso \code{\link{plotResidual}}
#'
#' @export
#' @rdname plotD3Residual

plotD3Residual <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                           std_residuals = FALSE,point_count = NULL, single_plot = TRUE,
                           scale_plot = FALSE, background = FALSE){

  if (points == FALSE & smooth == FALSE) stop("Plot points or smooth.")

  n <- length(list(...)) + 1

  aul <- list(object, ...)

  # chose y
  if (std_residuals == TRUE) {
    y <- "std.res"
    yTitle <- "Standardized residuals"
    chartTitle <- "Standardized residuals"
  } else {
    y <- "res"
    yTitle <- "Residuals"
    chartTitle <- "Residuals"
  }

  # make every input modelResiduals, check `variable`
  mrl <- list()
  varl <- c()

  for (i in 1:n) {
    object <- aul[[i]]

    if (!any(class(object) %in%  c("modelAudit","modelResiduals"))) stop("The function requires an object created with audit() or modelResiduals().")
    if (!("modelResiduals" %in% class(object))) {
      mr <- modelResiduals(object, variable)
    } else {
      mr <- object
    }

    varl <- c(varl, as.character(mr$variable[1]))

    df <- mr[, c(y, "val", "label")]
    class(df) <- "data.frame"
    colnames(df) <- c("y", "x", "label")
    mrl[[i]] <- df
  }

  if (length(unique(varl)) > 1) {
    stop("Objects have more than one variable name.")
  } else {
    variable <- varl[1]
    chartTitle <- paste0(chartTitle, " vs ", variable)
  }

  modelNames <- unlist(lapply(mrl, function(x) unique(x$label)))
  pointMax <- pointMin <- smoothMax <- smoothMin <- NULL
  pointData <- smoothData <- NA

  # prepare points data
  if (points == TRUE) {

    # find instance count and adjust point_count
    m <- dim(mrl[[1]])[1]
    if (is.null(point_count) || point_count > m) {
      pointData <- mrl
    } else {
      pointData <- lapply(mrl, function(mr) {
        mr <- mr[sample(m,point_count),]
        mr
      })
    }

    names(pointData) <- modelNames
    pointMax <- max(sapply(mrl, function(x) max(x$y)))
    pointMin <- min(sapply(mrl, function(x) min(x$y)))
  }

  # prepare smooth data
  if (smooth == TRUE) {

    smoothData <- lapply(mrl, function(mr) {
      model <- mgcv::gam(y ~ s(x, bs = "cs"), data = mr)
      vec <- data.frame(x = seq(min(mr$x), max(mr$x), length.out = 100))
      p <- predict(model, vec)
      df <- data.frame(x = vec$x, smooth = as.numeric(p))
      dim(df$x) <- NULL
      df
    })

    names(smoothData) <- modelNames
    smoothMax <- max(sapply(smoothData, function(x) max(x$smooth)))
    smoothMin <- min(sapply(smoothData, function(x) min(x$smooth)))
  }

  # find x and y scale
  xmax <- max(mrl[[1]]$x)
  xmin <- min(mrl[[1]]$x)
  ymax <- max(pointMax, smoothMax)
  ymin <- min(pointMin, smoothMin)

  ticksMargin <- abs(ymin-ymax)*0.15;

  temp <- jsonlite::toJSON(list(pointData, smoothData))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax + ticksMargin, ymin = ymin - ticksMargin,
                  variable = variable, n = n,
                  points = points, smooth = smooth, peaks = FALSE,
                  scalePlot = scale_plot, yTitle = yTitle, chartTitle = chartTitle)

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
           dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
           css = system.file("d3js/themeDrWhy.css", package = "auditor"),
           d3_version = 4,
           options = options)
  }
}

#' @rdname plotD3_prediction
#' @export
plotD3Residual <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                           std_residuals = FALSE,point_count = NULL, single_plot = TRUE,
                           scale_plot = FALSE, background = FALSE){
  message("Please note that 'plotD3Residual()' is now deprecated, it is better to use 'plotD3_residual()' instead.")
  plotD3_residual(object, ..., variable, points, smooth,
                  std_residuals, point_count, single_plot,
                  scale_plot, background)
}


