#' @title LIFT Chart
#'
#' @description LIFT is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class 'auditor_model_evaluation' created with \code{\link{model_evaluation}} function.
#' @param ... Other 'auditor_model_evaluation' objects to be plotted together.
#'
#' @return A ggplot object.
#'
#' @seealso \code{\link{model_evaluation}}
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
#' plot_lift(eva_glm)
#' plot(eva_glm, type ="lift")
#'
#' model_glm_2 <- glm(survived ~ .-age, family = binomial, data = titanic)
#' exp_glm_2 <- DALEX::explain(model_glm_2, data = titanic, y = titanic$survived, label = "glm2")
#' eva_glm_2 <- model_evaluation(exp_glm_2)
#'
#' plot_lift(eva_glm, eva_glm_2)
#' plot(eva_glm, eva_glm_2, type = "lift")
#'
#'
#' @import ggplot2
#'
#' @export
plot_lift <- function(object, ...) {
  # some safeguard
  `_rpp_` <- `_tp_` <- `_label_` <- variable <- line <- ord <- NULL
  # check if passed object is of class "auditor_model_evaluation"
  check_object(object, type = "eva")

  df1 <- make_dataframe(object, ..., type = "eva")

  # take only columns required to plot LIFT curve
  df1 <- df1[, c("_rpp_", "_tp_", "_label_")]
  df1$line <- "1"
  # prepare data frame for ideal and dummy model
  pr <- sum(object$`_y_` == levels(factor(object$`_y_`))[2]) / length(object$`_y_`)

  ideal_df <- data.frame(rpp = c(0, pr, 1),
                         tp = c(0, max(df1$`_tp_`), max(df1$`_tp_`)),
                         label = c("ideal", "ideal", "ideal"))

  random_df <- data.frame(rpp = c(0, 1),
                          tp =  c(0, max(df1$`_tp_`)),
                          label = c("random", "random"))
  df2 <- rbind(ideal_df, random_df)
  df2$line <- "2"
  colnames(df2)[1:3] <- c("_rpp_", "_tp_", "_label_")

  df <- rbind(df1, df2)
  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(factor(df$`_label_`))), df$`_label_`)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(unique(df1$`_label_`))))
  # main plot
  p <- ggplot(df, aes(x = `_rpp_`, y = `_tp_`)) +
    geom_line(aes(color = `_label_`, group = ord, linetype = line)) +
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
                       breaks = levels(df1$`_label_`),
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
  message("Please note that 'plotLIFT()' is now deprecated, it is better to use 'plot_lift()' instead.")
  plot_lift(object, ...)
}
