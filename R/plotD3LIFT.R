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
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm, data = titanic, y = titanic$survived)
#'
#' plotD3LIFT(audit_glm)
#'
#' model_glm_2 <- glm(survived ~ .-age, family = binomial, data = titanic)
#' audit_glm_2 <- audit(model_glm_2, data = titanic, y = titanic$survived, label = "glm2")
#'
#' plotD3LIFT(audit_glm, audit_glm_2, scale_plot = TRUE)
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

  df1 <- make_dataframe(object, ..., type = "eva")

  # take only columns required to plot LIFT curve
  df1 <- df1[ ,c("rpp", "tp","cutoffs", "label")]
  # prepare data frame for ideal and dummy model

  pr <- sum(object$y == levels(factor(object$y))[2]) / length(object$y)
  ideal_df <- data.frame(rpp = c(0, pr, 1),
                         tp = c(0, max(df1$tp), max(df1$tp)),
                         cutoffs = c(0,0,0),
                         label = c("ideal", "ideal", "ideal"))


  random_df <- data.frame(rpp = c(0, 1),
                          tp =  c(0, max(df1$tp)),
                          cutoffs = c(0,1),
                          label = c("random", "random"))

  df2 <- rbind(ideal_df, random_df)
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

  r2d3::r2d3(data = temp, script = system.file("d3js/plotLIFT.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}
