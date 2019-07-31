#' @title Receiver Operating Characteristic (ROC)
#'
#' @description Receiver Operating Characterstic Curve is a plot of the true positive rate (TPR) against the false positive rate (FPR) for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class 'model_audit' or 'model_evaluation'.
#' @param ... Other modelAudit objects to be plotted together.
#' @param nlabels Number of cutoff points to show on the plot. Default is `NULL`.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.model_audit}, \link{plotRROC}, \link{plotREC}}
#'
#' @import ggplot2
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' exp_glm <- DALEX::explain(model_glm, y = titanic$survived)
#' ev_glm <- model_evaluation(exp_glm)
#' plot_roc(ev_glm)
#'
#' @export


plot_roc <- function(object, ..., nlabels = NULL) {

  label <- fpr <- tpr <- ord <- cutoffs <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  # prepare data frame for the ggplot object
  df <- make_dataframe(object, ..., type = "eva")
  # if cutoff points should be placed on the chart
  n_models  <- length(unique(df$`_label_`))
  len_model <- nrow(df) / n_models
  inds <- c()
  if (!is.null(nlabels)) {
    inds <- floor(seq(1, len_model, length.out = nlabels))
    inds <- c(inds, as.vector(sapply(2:n_models, function(x) c(inds + (len_model * (x - 1))))))
  }

  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(df$`_label_`)), df$`_label_`)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(n_models))

  p <- ggplot(data = df, aes(x = `_fpr_`, y = `_tpr_`, color = `_label_`, group = ord)) +
    geom_step() +
    geom_point(data = df[inds,], show.legend = FALSE) +
    geom_text_repel(data = df[inds,], aes(label = format(round(cutoffs, 2), nsmall = 2)), show.legend = FALSE, size = 3.5)

  # theme, colours, titles, axes, scales, etc.
  p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$`_label_`), guide = guide_legend(nrow = 1)) +
    xlab("False positive fraction") +
    ylab("True positive fraction") +
    ggtitle("ROC curve")
}

#' @rdname plot_roc
#' @export
plotROC <- function(object, ..., nlabels = NULL) {
  message("Please note that 'plotROC()' is now deprecated, it is better to use 'plot_roc()' instead.")
  plot_roc(object, ..., nlabels)
}