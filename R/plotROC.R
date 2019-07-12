#' @title Receiver Operating Characteristic (ROC)
#'
#' @description Receiver Operating Characterstic Curve is a plot of the true positive rate (TPR) against the false positive rate (FPR) for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class ModelAudit or modelEvaluation.
#' @param ... Other modelAudit objects to be plotted together.
#' @param nlabels Number of cutoff points to show on the plot. Default is `NULL`.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}, \link{plotRROC}, \link{plotREC}}
#'
#' @import ggplot2
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm, y = titanic$survived)
#'
#' plotROC(audit_glm)
#'
#' @export


plotROC <- function(object, ..., nlabels = NULL) {

  label <- fpr <- tpr <- ord <- cutoffs <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  # prepare data frame for the ggplot object
  df <- make_dataframe(object, ..., type = "eva")

  # if cutoff points should be placed on the chart
  n_models  <- length(unique(df$label))
  len_model <- nrow(df) / n_models
  inds <- c()
  if (!is.null(nlabels)) {
    inds <- floor(seq(1, len_model, length.out = nlabels))
    inds <- c(inds, as.vector(sapply(2:n_models, function(x) c(inds + (len_model * (x - 1))))))
  }

  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(df$label)), df$label)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(n_models))

  p <- ggplot(data = df, aes(x = fpr, y = tpr, color = label, group = ord)) +
    geom_step() +
    geom_point(data = df[inds,], show.legend = FALSE) +
    geom_text_repel(data = df[inds,], aes(label = format(round(cutoffs, 2), nsmall = 2)), show.legend = FALSE, size = 3.5)

  # theme, colours, titles, axes, scales, etc.
  p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$label), guide = guide_legend(nrow = 1)) +
    xlab("False positive fraction") +
    ylab("True positive fraction") +
    ggtitle("ROC curve")
}
