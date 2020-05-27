#' @title LIFT Chart
#'
#' @description LIFT is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class \code{auditor_model_evaluation} created with \code{\link{model_evaluation}} function.
#' @param ... Other \code{auditor_model_evaluation} objects to be plotted together.
#' @param zeros Logical. It makes the lines start from the \code{(0,0)} point. By default it's \code{TRUE}.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{model_evaluation}}
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
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
#' plot_lift(eva_glm)
#' plot(eva_glm, type ="lift")
#'
#' model_glm_2 <- glm(survived ~ .-age, family = binomial, data = titanic_imputed)
#' glm_audit_2 <- audit(model_glm_2,
#'                      data = titanic_imputed,
#'                      y = titanic_imputed$survived,
#'                      label = "glm2")
#' eva_glm_2 <- model_evaluation(glm_audit_2)
#'
#' plot_lift(eva_glm, eva_glm_2)
#' plot(eva_glm, eva_glm_2, type = "lift")
#'
#'
#' @export
plot_lift <- function(object, ..., zeros = TRUE) {

  # some safeguard
  `_rpp_` <- `_tp_` <- `_label_` <- variable <- line <- `_ord_` <- NULL

  # check if passed object is of class "auditor_model_evaluation"
  check_object(object, type = "eva")

  df1 <- as.data.frame(object)

  for (resp in list(...)) {
    resp <- as.data.frame(resp)
    df1 <- rbind(df1, resp)
  }

  # take only columns required to plot LIFT curve
  df1 <- df1[, c("_rpp_", "_tp_", "_label_")]
  df1$line <- 1

  if (zeros && df1$`_rpp_`[1] != 0) {
    models <- levels(df1$`_label_`)
    df1$`_label_` <- as.numeric(df1$`_label_`)

    for (i in 1:length(models)) {
      df1 <- rbind(df1, c(0, 0, i, 1))
    }

    df1$line <- factor(df1$line)
    df1$`_label_` <- factor(df1$`_label_`, labels = models)
  }

  # set the orders of curves
  df1$`_ord_` <- 100 - as.numeric(df1$`_label_`)

  # prepare data frame for ideal and dummy model
  pr <- sum(object$`_y_` == levels(factor(object$`_y_`))[3]) / length(object$`_y_`)


  ideal_df <- data.frame(rpp = c(0, pr, 1),
                         tp = c(0, max(df1$`_tp_`), max(df1$`_tp_`)),
                         label = rep("ideal", 3),
                         ord = 1)

  random_df <- data.frame(rpp = c(0, 1),
                          tp =  c(0, max(df1$`_tp_`)),
                          label = c("random", "random"),
                          ord = 2)

  df2 <- rbind(ideal_df, random_df)
  colnames(df2) <- paste0("_", colnames(df2), "_")
  df2$line <- "2"

  df <- rbind(df1, df2)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(nlevels(df1$`_label_`)))

  # main plot
  p <- ggplot(df, aes(x = `_rpp_`, y = `_tp_`)) +
    geom_line(aes(color = `_label_`,
              group = factor(`_ord_`),
              linetype = `line`)) +
    xlab("Rate of positive prediction") +
    ylab("True positive") +
    ggtitle("LIFT Chart") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = FALSE)

  # X axis labels
  p <- p +
    scale_x_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0))

  # theme and colours
  p <- p + theme_drwhy() +
    scale_color_manual(values = c(rev(colours), "#4378bf", "#ae2c87"),
                       breaks = levels(`_label_`),
                       guide = guide_legend(nrow = 1)) +
    theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
          plot.title = element_text(margin = margin(b = 10)),
          legend.margin = margin(b = 15),
          axis.line.x = element_line(color = "#371ea3"))

  p + annotate("text", x = max(df$`_rpp_`) * 0.6, y = max(df$`_tp_`) * 0.1, size = 3.3, hjust = 0,
               label = "Upper dashed line: ideal model\nLower dashed line: random model")
}

#' @rdname plot_lift
#' @export
plotLIFT <- function(object, ...) {
  warning("Please note that 'plotLIFT()' is now deprecated, it is better to use 'plot_lift()' instead.")
  plot_lift(object, ...)
}
