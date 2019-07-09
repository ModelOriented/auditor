#' @title Plot LIFT in D3 with r2d3 package.
#'
#' @description LIFT is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class modelAudit or modelEvaluation.
#' @param ... Other modelAudit objects to be plotted together.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object
#'
#' @seealso \code{\link{plotLIFT}}
#'
#' @examples
#' library(mlbench)
#' library("auditor")
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#'
#' glm_model <- glm(diabetes ~ ., family = binomial, data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes, label = "glm1")
#'
#' plotD3LIFT(glm_au)
#'
#' glm_model2 <- glm(diabetes~pressure, family=binomial,	data=PimaIndiansDiabetes)
#' glm_au2 <- audit(glm_model2, data = Pima, y = Pima$diabetes, label = "glm2")
#'
#' plotD3LIFT(glm_au, glm_au2, scale_plot = TRUE)
#'
#' @export
#' @rdname plotD3LIFT

plotD3LIFT <- function(object, ..., scale_plot = FALSE) {

  # some safeguard
  rpp <- tp <- label <- NULL

  xTitle <- "Rate of positive prediction"
  yTitle <- "True positive"
  chartTitle <- "LIFT Chart"

  n <- length(list(object, ...))

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  modelNames <- lapply(list(object, ...), function(x) x$label)

  # prepare data frame for ideal and dummy model
  ideal_df <- attributes(modelEvaluation(object))$idealCGains
  ideal_df <- rbind(c(0, 0, 0, "ideal"), ideal_df)

  cols <- c("rpp", "tp", "alpha")
  ideal_df[,cols] = apply(ideal_df[,cols], 2, function(x) as.numeric(x))

  random_df <- data.frame(rpp = c(0, 1),
                          tp =  c(0, max(ideal_df$tp)),
                          alpha = c(0, 1),
                          label = c("random", "random"))

  df2 <- rbind(ideal_df, random_df)

  # prepare data frame for the main ggplot object
  df1 <- make_dataframe(object, ..., variable = NULL, type = "eva")

  df1[,cols] = apply(df1[,cols], 2, function(x) as.numeric(x))

  # new variable to set different style of line for ideal and dummy models
  df1$line <- "1"
  df2$line <- "2"
  df <- rbind(df2, df1)

  #:#
  ymax <- max(df$tp)
  ymin <- min(df$tp)

  lineData <- split(df, f = df$label)

  temp <- jsonlite::toJSON(list(lineData[c("ideal","random")],
                                lineData[setdiff(names(lineData), c("ideal","random"))]))

  options <- list(ymax = ymax, ymin = ymin,
                  scalePlot = scale_plot, n = n,
                  xTitle = xTitle, yTitle = yTitle, chartTitle = chartTitle)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotCurve.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}
