#' @title LIFT
#'
#' @description LIFT is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class modelAudit or modelEvaluation.
#' @param ... Other modelAudit objects to be plotted together.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @examples
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#' glm_model <- glm(diabetes ~ ., family = binomial, data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes)
#' plotLIFT(glm_au)
#'
#' @import ggplot2
#'
#'
#' @export
plotLIFT <- function(object, ...) {
  # some safeguard
  rpp <- tp <- label <- variable <- line <- NULL

  # check if passed object is of class "modelEvaluation" or "modelAudit"
  check_object(object, type = "eva")

  df1 <- make_dataframe(object, ..., type = "eva")
  # take only columns required to plot LIFT curve
  df1 <- df1[ ,c("rpp", "tp", "label")]
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

  # prepare data frame for the main ggplot object

  # df1 <- make_dataframe(object, ..., variable = variable, type = "eva")
  # for (lab in unique(df1$label)) df1 <- rbind(df1, c("0", "0", "0", lab))

  # df1[,cols] = apply(df1[,cols], 2, function(x) as.numeric(x))


  # new variable to set different style of line for ideal and dummy models
  df <- rbind(df1, df2)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main plot
  p1 <- ggplot(df, aes(x = rpp, y = tp)) +
    geom_line(aes(color = label, linetype = line)) +
    xlab("Rate of positive prediction") +
    ylab("True positive") +
    ggtitle("LIFT Chart") +
    scale_linetype_manual(values = c("solid", "dashed"), guide = FALSE)

  # X axis labels
  p1 <- p1 +
    scale_x_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0)) +
    scale_y_continuous(breaks = scales::pretty_breaks(), expand = c(0, 0))

  # theme and colours
  p1 <- p1 + theme_drwhy() +
    scale_color_manual(values = c(rev(colours), "#4378bf", "#ae2c87"),
                       breaks = levels(df1$label),
                       guide = guide_legend(nrow = 1)) +
    theme(plot.margin = unit(c(0, 0.5, 0, 0), "cm"),
          plot.title = element_text(margin = margin(b = 10)),
          legend.margin = margin(b = 15),
          axis.line.x = element_line(color = "#371ea3"))

  # plot of ideal and dummy models - just to get the legend
  p2 <- ggplot(data = df2, aes(x = rpp, y = tp)) +
    geom_line(aes(colour = label), linetype = "dashed") +
    scale_color_manual(values = c("#4378bf", "#ae2c87")) +
    theme_drwhy() +
    theme(legend.position = c(0.9, 0.1)) +
    guides(colour = guide_legend(nrow = 2, byrow = TRUE))

  p2_tab <- ggplot_gtable(ggplot_build(p2))
  p2_ind <- which(sapply(p2_tab$grobs, function(x) x$name) == "guide-box")
  p2_leg <- p2_tab$grobs[[p2_ind]]

  return(grid.arrange(p1,
                      arrangeGrob(p2_leg, nrow = 2, heights = c(unit(0.8, "npc"), unit(0.2, "npc")),
                                  widths = unit(0.5, "npc")), ncol = 2, widths = c(8, 0)))
}
