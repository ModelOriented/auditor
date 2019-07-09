#' @title Autocorrelation Plot in D3 with r2d3 package.
#'
#' @description Plot of i-th residual vs i+1-th residual.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit objects. Name of model variable to order residuals.
#' If value is NULL the data is ordered by a vector of actual response (\code{y} parameter
#' passed to the \code{\link{audit}} function). One can also pass any name of any other variable
#' in the data set. If \code{variable = ""} is set, unordered observations are presented.
#' @param points Logical, indicates whenever observations should be added as points. By defaul it's TRUE.
#' @param smooth Logical, indicates whenever smoothed lines should be added. By default it's FALSE.
#' @param point_count Number of points to be plotted per model. Points will be chosen randomly. By default plot all of them.
#' @param single_plot Logical, indicates whenever single or facets should be plotted. By default it's TRUE.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#' @param background Logical, available only if single_plot = FALSE. Indicates whenever backgroud plots should be plotted. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#' @examples
#' library(auditor)
#' library(DALEX)
#'
#' lm_model <- lm(m2.price ~ ., data = apartments)
#'
#' library("randomForest")
#' rf_model <- randomForest(m2.price ~ ., data = apartments)
#'
#' lm_au <- audit(lm_model, label = "lm", data = apartments, y = apartments$m2.price)
#' rf_au <- audit(rf_model, label = "rf", data = apartments, y = apartments$m2.price)
#'
#' lm_mr_year <- modelResiduals(lm_au, variable = "construction.year")
#' rf_mr_year <- modelResiduals(rf_au, variable = "construction.year")
#'
#' plotD3Autocorrelation(rf_mr_year, smooth = FALSE, scale_plot = TRUE, point_count = 1000)
#'
#' plotD3Autocorrelation(rf_mr_year, lm_mr_year, smooth = TRUE)
#' plotD3Autocorrelation(rf_mr_year, lm_mr_year, smooth = TRUE, single_plot = FALSE)
#'
#' @export
#' @rdname plotD3Autocorrelation
plotD3Autocorrelation <- function(object, ..., variable = NULL, points = TRUE, smooth = FALSE,
                                  point_count = NULL, single_plot = TRUE, scale_plot = FALSE,
                                  background = FALSE) {

  if (points == FALSE & smooth == FALSE) stop("Plot points or smooth.")

  n <- length(list(...)) + 1

  aul <- list(object, ...)

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

    ind <- dim(mr)[1]

    mrl[[i]] <- data.frame(x = mr$res[-ind], y = mr$res[-1], label = mr$label[-1])
  }

  if (length(unique(varl)) > 1) {
    stop("Objects have more than one variable name.")
  } else {
    variable <- varl[1]
    chartTitle <- paste("Autocorrelation plot by", variable)
  }

  xTitle <- "residual i"
  yTitle <- "residual i+1"

  modelNames <- unlist(lapply(mrl, function(x) unique(x$label)))
  xMin <- xMax <- yMin <- yMax <- NULL
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
  }

  # prepare smooth data
  if (smooth == TRUE) {

    smoothData <- lapply(mrl, function(mr) {
      model <- mgcv::gam(y ~ s(x, bs = "cs"), data = mr)
      vec <- data.frame(x = seq(min(mr$x), max(mr$x), length.out = 100))
      p <- predict(model, vec)
      df <- data.frame(val = vec$x, smooth = as.numeric(p))
      dim(df$val) <- NULL
      df
    })

    names(smoothData) <- modelNames
  }

  xmax <- max(sapply(mrl, function(x) max(x$x, x$y)))
  xmin <- min(sapply(mrl, function(x) min(x$x, x$y)))
  ymax <- ifelse(all(is.na(smoothData)), xmax, max(sapply(smoothData, function(x) max(x$smooth)), xmax))
  ymin <- ifelse(all(is.na(smoothData)), xmin, min(sapply(smoothData, function(x) min(x$smooth)), xmin))

  temp <- jsonlite::toJSON(list(pointData, smoothData))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax, ymin = ymin,
                  variable = variable, n = n,
                  points = points, smooth = smooth,
                  scalePlot = scale_plot, yTitle = yTitle, xTitle = xTitle,
                  chartTitle = chartTitle)

  if (single_plot == TRUE) {

    r2d3::r2d3(data = temp, script = system.file("d3js/plotAutocorrelationSingle.js", package = "auditor"),
               dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)

  } else {
    if (n==1) stop("Use single_plot instead.")
    options['background'] <- background

    r2d3::r2d3(data = temp, script = system.file("d3js/plotAutocorrelationMany.js", package = "auditor"),
               dependencies = system.file("d3js/colorsDrWhy.js", package = "auditor"),
               css = system.file("d3js/themeDrWhy.css", package = "auditor"),
               d3_version = 4,
               options = options)
  }
}
