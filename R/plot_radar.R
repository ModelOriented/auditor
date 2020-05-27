#' @title Model Ranking Plot
#'
#' @description Radar plot with model score. score are scaled to \code{[0,1]},
#' each score is inversed and divided by maximum score value.
#'
#' @param object An object of class \code{auditor_model_performance} created with \code{\link{model_performance}} function.
#' @param ... Other \code{auditor_model_performance} objects to be plotted together.
#' @param verbose Logical, indicates whether values of scores should be printed.
#'
#' @return A ggplot object.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' mp_lm <- model_performance(lm_audit)
#'
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#' mp_rf <- model_performance(rf_audit)
#'
#' # plot results
#' plot_radar(mp_lm, mp_rf)
#'
#' @export
plot_radar <- function(object, ..., verbose = TRUE) {

  # safeguard
  x <- y <- `_value_` <- scaled <- `_name_` <- `_label_` <- `_score_` <- label <- name <- value <- NULL

  # check if passed object is of class "model_performance"
  check_object(object, type = "prfm")
  # data frame for ggplot object

  df <- make_dataframe(object, ..., type = "prfm")

  # data frame for extra geoms
  df_text <- data.frame(x = df[,"_name_"][1], y = c(0.01, 0.25, 0.50, 0.75, 1), label = seq(0, 1, 0.25))

  # colors for model(s)
  colours <- rev(theme_drwhy_colors(nlevels(df$`_label_`)))

  # plot
  p <- ggplot(data = df, aes(x = `_name_`, y = `_score_`)) +
    coord_radar(names_n = length(unique(df$`_name_`))) +
    geom_polygon(aes(group = `_label_`, color = `_label_`), fill = NA, show.legend = FALSE) +
    geom_line(aes(group = `_label_`, color = `_label_`)) +
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

  if (verbose == TRUE) {
    df$`_name_` <- gsub("inv\n", "", df$`_name_`)
    colnames(df) <- gsub("_", "", colnames(df))
    df <- subset(df, select = -score)
    print(subset(df, select = c(name, label, value, scaled)))
  }
  p
}

#' @rdname plot_radar
#' @export
plotModelRanking <- function(object, ..., verbose = TRUE) {
  warning("Please note that 'plotModelRanking()' is now deprecated, it is better to use 'plot_radar()' instead.")
  plot_radar(object, ..., verbose = verbose)
}

