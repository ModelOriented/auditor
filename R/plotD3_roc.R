#' @title Receiver Operating Characteristic (ROC) in D3 with r2d3 package.
#'
#' @description Receiver Operating Characterstic Curve is a plot of the true positive rate (TPR) against the false positive rate (FPR) for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class 'auditor_model_evaluation' created with \code{\link{model_evaluation}} function.
#' @param ... Other 'auditor_model_evaluation' objects to be plotted together.
#' @param nlabel Number of cutoff points to show on the plot. Default is `NULL`.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's FALSE.
#'
#' @return a `r2d3` object
#'
#' @seealso \code{\link{plot_roc}}
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- as.numeric(titanic$survived == "yes")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm, y = titanic$survived)
#'
#' # validate a model with auditor
#' library(auditor)
#' eva_glm <- model_evaluation(exp_glm)
#'
#' # plot results
#' plotD3_roc(eva_glm)
#'
#' #add second model
#' model_glm_2 <- glm(survived ~ .-age, family = binomial, data = titanic)
#' exp_glm_2 <- DALEX::explain(model_glm_2, data = titanic, y = titanic$survived, label = "glm2")
#' eva_glm_2 <- model_evaluation(exp_glm_2)
#'
#' plotD3_roc(eva_glm, eva_glm_2)
#'
#' @export
#' @rdname plotD3_roc
plotD3_roc <- function(object, ..., nlabel = NULL, scale_plot = FALSE) {

  x_title <- "False positive fraction"
  y_title <- "True positive franction"
  chart_title <- "ROC Curve"

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  # prepare data frame for the ggplot object
  df <- make_dataframe(object, ..., type = "eva")
  # if cutoff points should be placed on the chart
  n_models  <- length(unique(df$`_label_`))
  len_model <- nrow(df) / n_models
  inds <- c()
  if (!is.null(nlabel)) {
    inds <- floor(seq(1, len_model, length.out = nlabel))
    inds <- as.vector(sapply(1:n_models, function(x) c(inds + (len_model * (x - 1)))))
  }

  # new varibale to set an order o curves
  df$`_label_` <- factor(df$`_label_`)

  df <- as.data.frame(df[,c('_fpr_','_tpr_','_cutoffs_','_label_')])
  colnames(df) <- c("fpr","tpr","curoffs","label")
  df$big <- FALSE
  df$big[inds] <- TRUE

  line_data <- split(df, f = df$label)

  temp <- jsonlite::toJSON(list(line_data))

  options <- list(scalePlot = scale_plot, n = n_models,
                  xTitle = x_title, yTitle = y_title,
                  chartTitle = chart_title)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotROC.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/tooltipD3.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}
