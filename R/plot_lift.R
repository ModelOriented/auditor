#' @title LIFT Chart
#'
#' @description LIFT is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class 'auditor_model_evaluation' created with \code{\link{model_evaluation}} function.
#' @param ... Other 'auditor_model_evaluation' objects to be plotted together.
#'
#' @return A ggplot object.docum
#'
#' @seealso \code{\link{model_evaluation}}
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' exp_glm <- DALEX::explain(model_glm, data = titanic, y = titanic$survived)
#' library(auditor)
#' eva_glm <- model_evaluation(exp_glm)
#' plot_lift(eva_glm)
#'
#' model_glm_2 <- glm(survived ~ .-age, family = binomial, data = titanic)
#' exp_glm_2 <- DALEX::explain(model_glm_2, data = titanic, y = titanic$survived, label = "glm2")
#' eva_glm_2 <- model_evaluation(exp_glm_2)
#'
#' plot_lift(eva_glm, eva_glm_2)
#'
#'
#' @import ggplot2
#'
#' @export
plot_lift <- function(object, ...) {
  # some safeguard
  rpp <- tp <- label <- variable <- line <- ord <- NULL
  # check if passed object is of class "auditor_model_evaluation"
  check_object(object, type = "eva")

  df1 <- make_dataframe(object, ..., type = "eva")

  # take only columns required to plot LIFT curve
  df1 <- df1[, c("rpp", "tp", "label")]
  df1$line <- "1"

  # prepare data frame for ideal and dummy model
  pr <- sum(object$y == levels(factor(object$y))[2]) / length(object$y)
  ideal_df <- data.frame(rpp = c(0, pr, 1),
                         tp = c(0, max(df1$tp), max(df1$tp)),
                         label = c("ideal", "ideal", "ideal"))

  random_df <- data.frame(rpp = c(0, 1),
                          tp =  c(0, max(df1$tp)),
                          label = c("random", "random"))

  df2 <- rbind(ideal_df, random_df)
  df2$line <- "2"

  df <- rbind(df1, df2)

  # new varibale to set an order o curves
  df$ord <- paste(rev(as.numeric(df$label)), df$label)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(unique(df1$label))))

  # main plot
  p <- ggplot(df, aes(x = rpp, y = tp)) +
    geom_line(aes(color = label, group = ord, linetype = line)) +
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
                       breaks = levels(df1$label),
                       guide = guide_legend(nrow = 1)) +
    theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
          plot.title = element_text(margin = margin(b = 10)),
          legend.margin = margin(b = 15),
          axis.line.x = element_line(color = "#371ea3"))

  p + annotate("text", x = max(df$rpp) * 0.6, y = max(df$tp) * 0.1, size = 3.3, hjust = 0,
               label = "Upper dashed line: ideal model\nLower dashed line: random model")
}

#' @rdname plot_lift
#' @export
plotLIFT <- function(object, ...) {
  message("Please note that 'plotLIFT()' is now deprecated, it is better to use 'plot_lift()' instead.")
  plot_lift(object, ...)
}
