#' @title Plot LIFT in D3 with r2d3 package.
#'
#' @description LIFT is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class 'auditor_model_evaluation' created with \code{\link{model_evaluation}} function.
#' @param ... Other 'auditor_model_evaluation' objects to be plotted together.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object
#'
#' @seealso \code{\link{plot_lift}}
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- titanic$survived == "yes"
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm, data = titanic, y = titanic$survived)
#'
#' # validate a model with auditor
#' library(auditor)
#' eva_glm <- model_evaluation(exp_glm)
#'
#' # plot results
#' plotD3_lift(eva_glm)
#'
#' model_glm_2 <- glm(survived ~ .-age, family = binomial, data = titanic)
#' exp_glm_2 <- DALEX::explain(model_glm_2, data = titanic, y = titanic$survived, label = "glm2")
#' eva_glm_2 <- model_evaluation(exp_glm_2)
#'
#' plotD3_lift(eva_glm, eva_glm_2)
#'
#' @export
#' @rdname plotD3_lift

plotD3_lift <- function(object, ..., scale_plot = FALSE) {

  # some safeguard
  rpp <- tp <- label <- NULL

  x_title <- "Rate of positive prediction"
  y_title <- "True positive"
  chart_title <- "LIFT Chart"

  n <- length(list(object, ...))

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  df1 <- make_dataframe(object, ..., type = "eva")

  # take only columns required to plot LIFT curve
  df1 <- df1[, c("_rpp_", "_tp_","_cutoffs_", "_label_")]
  colnames(df1) <- c("rpp","tp","cutoffs","label")

  # prepare data frame for ideal and dummy model

  pr <- sum(object$`_y_` == levels(factor(object$`_y_`))[2]) / length(object$`_y_`)
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

  line_data <- split(df, f = df$label)

  temp <- jsonlite::toJSON(list(line_data[c("ideal","random")],
                                line_data[setdiff(names(line_data), c("ideal","random"))]))

  options <- list(ymax = ymax, ymin = ymin,
                  scalePlot = scale_plot, n = n,
                  xTitle = x_title, yTitle = y_title, chartTitle = chart_title)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotLIFT.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/d3-tip.js", package = "auditor"),
               system.file("d3js/hackHead.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}


#' @rdname plotD3_lift
#' @export
plotD3LIFT <- function(object, ..., scale_plot = FALSE) {
  message("Please note that 'plotD3LIFT()' is now deprecated, it is better to use 'plotD3_lift()' instead.")
  plotD3_lift(object, ..., scale_plot)
}
