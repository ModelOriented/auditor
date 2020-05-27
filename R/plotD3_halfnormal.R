#' @title Plot Half-Normal in D3 with r2d3 package.
#'
#' @description The half-normal plot is one of the tools designed to evaluate the goodness of fit of a
#' statistical models. It is a graphical method for comparing two probability distributions by plotting
#' their quantiles against each other.
#' Points on the plot correspond to ordered absolute values of model diagnostic
#' (i.e. standardized residuals) plotted against theoretical order statistics from a half-normal distribution.
#'
#' @param object An object of class 'auditor_model_halfnormal' created with \code{\link{model_halfnormal}} function.
#' @param ... Other 'auditor_model_halfnormal' objects.
#' @param quantiles If TRUE values on axis are on quantile scale.
#' @param sim Number of residuals to simulate.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a \code{r2d3} object
#'
#' @seealso \code{\link{model_halfnormal}}
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
#' hn_lm <- model_halfnormal(lm_audit)
#'
#' # plot results
#' plotD3_halfnormal(hn_lm)
#'
#' @importFrom stats ecdf dnorm density
#'
#' @seealso \code{\link{score_halfnormal}, \link{plot_halfnormal}}
#'
#' @export
#' @rdname plotD3_halfnormal

plotD3_halfnormal <- function(object, ..., quantiles = FALSE, sim = 99, scale_plot = FALSE) {

  # some safeguard
  x <- residuals <- upper <- lower <- NULL

  x_title <- "Half-normal Quantiles"
  chart_title <- "Half-normal plot"
  y_title <- ifelse(quantiles==TRUE, "Quantiles of |residuals|", "|Residuals|")

  n <- length(list(object, ...))

  # check if passed object is of class "modelFit"
  check_object(object, type = "fit")

  df <- make_dataframe(object, ..., quant = quantiles, type = "fit")

  df <- df[,c("_x_","_lower_","_median_","_upper_","_residuals_","_label_")]
  colnames(df) <- c("x","lower","median","upper","residuals","label")

  plot_data <- split(df, f = df$label)

  x_min_max <- lapply(plot_data, function(x){
    range(x$x)
  })

  y_min_max <- lapply(plot_data, function(x){
    range(x[, setdiff(colnames(x), c("x","label"))])
  })

  temp <- jsonlite::toJSON(list(plot_data, x_min_max, y_min_max))

  options <- list(scalePlot = scale_plot, n = n,
                  xTitle = x_title, yTitle = y_title,
                  chartTitle = chart_title)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotHalfNormalMany.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/hackHead.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}


#' @rdname plotD3_halfnormal
#' @export
plotD3HalfNormal <- function(object, ..., quantiles = FALSE, sim = 99, scale_plot = FALSE) {
  warning("Please note that 'plotD3HalfNormal()' is now deprecated, it is better to use 'plotD3_halfnormal()' instead.")
  plotD3_halfnormal(object, ..., quantiles, sim, scale_plot)
}
