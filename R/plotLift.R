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
#' @importFrom ROCR performance prediction
#'
#'
#' @export
plotLIFT <- function(object, ...) {
  # some safeguard
  rpp <- tp <- label <- NULL

  # check if passed object is of class "modelResiduals" or "modelAudit"
  check_object(object, type = "eva")

  # prepare data frame for ideal model
  ideal_df <- attributes(modelEvaluation(object))$idealCGains
  ideal_df <- rbind(ideal_df, c(0, 0, 0, "ideal"))

  cols <- c("rpp", "tp", "alpha")
  ideal_df[,cols] = apply(ideal_df[,cols], 2, function(x) as.numeric(x))

  # prepare data frame for dummy model
  random_df <- data.frame(rpp = c(0, 1),
                          tp =  c(0, max(ideal_df$tp)),
                          alpha = c(0, 1),
                          label = c("random", "random"))

  # prepare data frame for ggplot object
  df <- make_dataframe(object, ..., variable = variable, type = "eva")
  for (lab in unique(df$label)) df <- rbind(df, c("0", "0", "0", lab))

  df[,cols] = apply(df[,cols], 2, function(x) as.numeric(x))

  # make new variable to keep order of lines correct
  df$ord <- paste(rev(as.numeric(df$label)), df$label)

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(levels(df$label))))

  # main plot
  p <- ggplot(df, aes(x = rpp, y = tp)) +
    geom_line(data = ideal_df,  aes(rpp, tp), color = "#4378bf", linetype = "dashed") +
    geom_line(data = random_df, aes(rpp, tp), color = "#ae2c87", linetype = "dashed") +
    geom_line(aes(group = ord, color = label)) +
    xlab("Rate of positive prediction") +
    ylab("True positive") +
    ggtitle("LIFT Chart")

  # theme and colours
  p <- p + theme_drwhy() +
    theme(axis.line.x = element_line(color = "#371ea3")) +
    scale_color_manual(values = rev(colours))

  # X axis labels
  p <- p + scale_x_continuous(breaks = scales::pretty_breaks())

  # extra legend for ideal and dummy model
  dashes  <- paste(rep("\U2212", 2), collapse = " ")
  x_coord_u <- 0.70
  x_coord_d <- 0.78
  y_coord   <- max(df$tp)
  y_coord_l <- y_coord * 0.10
  y_coord_r <- y_coord * 0.05
  p <- p + annotate("text", x = x_coord_u, y = y_coord_l, label = dashes, colour = "#4378bf", hjust = 0) +
    annotate("text", x = x_coord_d, y = y_coord_l, label = "ideal model", colour = "#160e3b", hjust = 0) +
    annotate("text", x = x_coord_u, y = y_coord_r, label = dashes, colour = "#ae2c87", hjust = 0) +
    annotate("text", x = x_coord_d, y = y_coord_r, label = "dummy model", colour = "#160e3b", hjust = 0)

  return(p)
}
