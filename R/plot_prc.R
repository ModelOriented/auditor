#' @title Precision-Recall Curve (PRC)
#'
#' @description Precision-Recall Curve summarize the trade-off between the true positive rate and the positive predictive
#' value for a model. It is useful for measuring performance and comparing classificators.
#'
#' @param object An object of class \code{auditor_model_evaluation} created with \code{\link{model_evaluation}} function.
#' @param ... Other \code{auditor_model_evaluation} objects to be plotted together.
#' @param nlabel Number of cutoff points to show on the plot. Default is \code{NULL}.
#'
#' @return A ggplot object.
#'
#' @examples
#' library(DALEX)
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' eva_glm <- model_evaluation(glm_audit)
#'
#' # plot results
#' plot_prc(eva_glm)
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
#' plot_prc(eva_glm, eva_glm_2)
#' plot(eva_glm, eva_glm_2)
#'
#' @rdname plot_roc
#' @export
plot_prc <- function(object, ..., nlabel = NULL) {

  `_label_` <- `_fpr_` <- `_tpr_` <- ord <- `_cutoffs_` <- `_recall_` <- `_precision_` <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  # prepare data frame for the ggplot object
  df <- as.data.frame(object)

  for (resp in list(...)) {
    resp <- as.data.frame(resp)
    df <- rbind(df, resp)
  }

  # if cutoff points should be placed on the chart
  n_models <- nlevels(df$`_label_`)
  len_model <- nrow(df) / n_models
  inds <- c()
  if (!is.null(nlabel)) {
    inds <- floor(seq(1, len_model, length.out = nlabel))
    inds <- as.vector(sapply(1:n_models, function(x) c(inds + (len_model * (x - 1)))))
  }


  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(factor(df$`_label_`))), df$`_label_`)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(n_models))
  p <- ggplot(data = df, aes(x = `_recall_`, y = `_precision_`, color = `_label_`, group = ord)) +
    geom_step() +
    geom_point(data = df[inds,], show.legend = FALSE) +
    geom_text_repel(data = df[inds,], aes(label = format(round(`_cutoffs_`, 2), nsmall = 2)), show.legend = FALSE, size = 3.5)

  # theme, colours, titles, axes, scales, etc.
  p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours), breaks = levels(df$`_label_`), guide = guide_legend(nrow = 1)) +
    coord_fixed() +
    xlab("Recall") +
    ylab("Precision") +
    ggtitle("ROC curve")
}
