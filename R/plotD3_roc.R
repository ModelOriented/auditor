#' @title Receiver Operating Characteristic (ROC) in D3 with r2d3 package.
#'
#' @description Receiver Operating Characteristic Curve is a plot of the true positive rate (TPR)
#'  against the false positive rate (FPR) for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class \code{auditor_model_evaluation} created with \code{\link{model_evaluation}} function.
#' @param ... Other \code{auditor_model_evaluation} objects to be plotted together.
#' @param nlabel Number of cutoff points to show on the plot. Default is \code{NULL}.
#' @param scale_plot Logical, indicates whenever the plot should scale with height. By default it's \code{FALSE}.
#'
#' @return a \code{r2d3} object
#'
#' @seealso \code{\link{plot_roc}}
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' # use DALEX package to wrap up a model into explainer
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' eva_glm <- model_evaluation(glm_audit)
#'
#' # plot results
#' plot_roc(eva_glm)
#' plot(eva_glm)
#'
#' #add second model
#' model_glm_2 <- glm(survived ~ .-age, family = binomial, data = titanic_imputed)
#' glm_audit_2 <- audit(model_glm_2,
#'                      data = titanic_imputed,
#'                      y = titanic_imputed$survived,
#'                      label = "glm2")
#' eva_glm_2 <- model_evaluation(glm_audit_2)
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
  df <- as.data.frame(object)

  for (resp in list(...)) {
    resp <- as.data.frame(resp)
    df <- rbind(df, resp)
  }

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

  if (df$tpr[1] != 0) {
    models <- levels(df$label)
    df$label <- as.numeric(df$label)

    for (i in 1:length(models)) {
      df <- rbind(df, c(0, 0, 0, i))
    }

    df <- df[order(df$label, df$tpr), ]
    df$label <- factor(df$label, labels = models)
  }


  line_data <- split(df, f = df$label)

  temp <- jsonlite::toJSON(list(line_data))

  options <- list(scalePlot = scale_plot, n = n_models,
                  xTitle = x_title, yTitle = y_title,
                  chartTitle = chart_title)

  r2d3::r2d3(data = temp, script = system.file("d3js/plotROC.js", package = "auditor"),
             dependencies = list(
               system.file("d3js/colorsDrWhy.js", package = "auditor"),
               system.file("d3js/d3-tip.js", package = "auditor"),
               system.file("d3js/hackHead.js", package = "auditor")
             ),
             css = system.file("d3js/themeDrWhy.css", package = "auditor"),
             d3_version = 4,
             options = options)
}
