#' @title Plot Autocorrelation Function in D3 with r2d3 package.
#'
#' @description Plot Autocorrelation Function of models residuals.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or modelResiduals objects to be plotted together.
#' @param variable Only for modelAudit object. Name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#' @param alpha Confidence level of the interval.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotD3ACF(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plotD3ACF(lm_au, rf_au)
#'
#' @importFrom stats qnorm acf
#'
#' @export
#' @rdname plotD3ACF

plotD3ACF <- function(object, ..., variable = NULL, alpha = 0.95, scale_plot = FALSE) {

  # some safeguard
  lag <- acf <- ymin <- NULL

  xTitle <- ifelse(!is.null(variable) && nchar(variable) > 1, paste0("Lag of ", variable), "")
  chartTitle <- "ACF plot"

  n <- length(list(object, ...))

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "res")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "res")

  resultDF <- data.frame(acf = numeric(), label = character(), lag = numeric(), ymin = numeric())
  for (label in unique(df$label)) {
    orderedResiduals <- df[which(df$label == label), "res"]
    acf <- acf(orderedResiduals, plot = FALSE)
    resultDF <- rbind(resultDF, data.frame(acf = acf$acf[-1], label = label, lag = acf$lag[-1], ymin = 0))
  }

  conf_lims <- c(-1, 1) * qnorm((1 + alpha) / 2) / sqrt(nrow(df))

  #:#
  xmax <- max(resultDF$lag)+1
  xmin <- min(resultDF$lag)-1
  ymax <- max(resultDF$acf, conf_lims,resultDF$ymin)
  ymin <- min(resultDF$acf, conf_lims,resultDF$ymin)

  yMargin <- (abs(ymax-ymin))*0.05

  lineData <- split(resultDF, f = resultDF$label)

  temp <- jsonlite::toJSON(list(lineData, conf_lims))

  options <- list(xmax = xmax, xmin = xmin,
                  ymax = ymax + yMargin, ymin = ymin - yMargin,
                  scalePlot = scale_plot, n = n,
                  xTitle = xTitle, chartTitle = chartTitle)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotACFMany.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}
