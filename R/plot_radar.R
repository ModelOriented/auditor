#' @title Model Ranking Plot
#'
#' @description Radar plot with model score. score are scaled to [0,1], each score is inversed and divided by maximum score value.
#'
#' @param object An object of class ModelAudit.
#' @param ... Other model_audit objects to be plotted together.
#' @param score Vector of score names to be plotted.
#' @param new_score A named list of functions that take one argument: object of class ModelAudit and return a numeric value. The measure calculated by the function should have the property that lower score value indicates better model.
#' @param print logical, should values of scores be printed?
#'
#' @return ggplot object
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- DALEX::explain(lm_model, data = dragons, y = dragons$life_length)
#' lm_mp <- model_performance(lm_exp)
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_exp <- DALEX::explain(rf_model, data = dragons, y = dragons$life_length)
#' rf_mp <- model_performance(rf_exp)
#'
#' plot_radar(lm_mp, rf_mp)
#'
#' @seealso \code{\link{plot.model_audit}}
#'
#' @import ggplot2
#' @import gridExtra
#' @importFrom grDevices blues9
#' @importFrom grid grobTree
#' @import scales
#'
#' @export
plot_radar <- function(object, ..., score = c("mae", "mse", "rec", "rroc"), new_score = NULL, print = TRUE) {

  # safeguard
  x <- y <- value <- scaled <- name <- label <- NULL
  # name <- score <- label <- x2 <- y2 <- label2 <- NULL

  # check if passed object is of class "model_performance"
  check_object(object, type = "prfm")
  # data frame for ggplot object

  df <- make_dataframe(object, ..., score = score, new_score = new_score, type = "prfm")

  # data frame for extra geoms
  df_text <- data.frame(x = df$name[1], y = c(0.01, 0.25, 0.50, 0.75, 1), label = seq(0, 1, 0.25))

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(length(unique(df$label))))

  # plot
  p <- ggplot(data = df, aes(x = name, y = score)) +
    coord_radar(names_n = length(unique(df$name))) +
    geom_polygon(aes(group = label, color = label), fill = NA, show.legend = FALSE) +
    geom_line(aes(group = label, color = label)) +
    geom_text(data = df_text, aes(x = x, y = y, label = label), size = 3, fontface = "bold") +
    scale_y_continuous(expand = c(0, 0), limits = c(0.01, 1)) +
    scale_color_manual(values = c(rev(colours)), guide = guide_legend(ncol = 1), name = "") +
    xlab("") +
    ylab("") +
    ggtitle("Model ranking radar") +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.text.x = element_text(size = 10),
          plot.title = element_text(color = "#371ea3", face = "bold", hjust = 0.5))

  df$name <- gsub("inv\n", "", df$name)

  if(print == TRUE) print(subset(df, select = c(name, label, value, scaled)))

   p
}

#' @rdname plot_radar
#' @export
plotModelRanking <- function(object, ..., score = c("MAE", "MSE", "REC", "RROC"), new_score = NULL) {
  message("Please note that 'plotModelRanking()' is now deprecated, it is better to use 'plot_radar()' instead.")
  plot_radar(object, ..., score, new_score)
}

