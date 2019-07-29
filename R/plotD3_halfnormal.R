#' @title Plot Half-Normal in D3 with r2d3 package.
#'
#' @description The half-normal plot is one of the tools designed to evaluate the goodness of fit of a
#' statistical models. It is a graphical method for comparing two probability distributions by plotting
#' their quantiles against each other.
#' Points on the plot correspond to ordered absolute values of model diagnostic
#' (i.e. standardized residuals) plotted against theoretical order statistics from a half-normal distribution.
#'
#' @param object 'model_audit' object, 'model_halfnormal' object.
#' @param quantiles if TRUE values on axis are on quantile scale.
#' @param ... extra arguments passed to \link[hnp]{hnp}.
#' @param sim number of residuals to simulate
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object.
#'
#' @examples
#' library(auditor)
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length, label = "lm")
#'
#' plotD3_halfnormal(lm_au)
#'
#' @importFrom hnp hnp
#' @importFrom stats ecdf dnorm density
#'
#' @seealso \code{\link{score_halfnormal}, \link{plot_halfnormal}}
#'
#' @export
#' @rdname plotD3_halfnormal

plotD3_halfnormal <- function(object, ..., quantiles = FALSE, sim = 99, scale_plot = FALSE) {

  # some safeguard
  x <- residuals <- upper <- lower <- NULL

  xTitle <- "Half-normal Quantiles"
  chartTitle <- "Half-normal plot"
  yTitle <- ifelse(quantiles==TRUE, "Quantiles of |residuals|", "|Residuals|")

  n <- length(list(object, ...))

  # check if passed object is of class "modelFit" or "modelAudit"
  check_object(object, type = "fit")

  # data frame for ggplot object
  df <- make_dataframe(object, ..., quant = quantiles, type = "fit")

  df <- df[,c("x","lower","median","upper","residuals","label")]

  plotData <- split(df, f = df$label)

  xMinMax <- lapply(plotData, function(x){
    range(x$x)
  })
  yMinMax <- lapply(plotData, function(x){
    range(x[,setdiff(colnames(x), c("x","label"))])
  })

  temp <- jsonlite::toJSON(list(plotData, xMinMax, yMinMax))

  options <- list(scalePlot = scale_plot, n = n,
                  xTitle = xTitle, yTitle = yTitle,
                  chartTitle = chartTitle)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotHalfNormalMany.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}


#' @rdname plotD3_halfnormal
#' @export
plotD3HalfNormal <- function(object, ..., quantiles = FALSE, sim = 99, scale_plot = FALSE) {
  message("Please note that 'plotD3HalfNormal()' is now deprecated, it is better to use 'plotD3_halfnormal()' instead.")
  plotD3_halfnormal(object, ..., quantiles, sim, scale_plot)
}
