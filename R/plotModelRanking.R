#' @title Model Ranking Plot
#'
#' @description Radar plot with model scores. Scores are scaled to [0,1], each score is inversed and divided by maximum score value.
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param scores Vector of score names to be plotted.
#' @param new.score A named list of functions that take one argument: object of class ModelAudit and return a numeric value. The measure calculated by the function should have the property that lower score value indicates better model.
#' @param table Logical. Specifies if rable with score values should be plotted
#'
#' @return ggplot object
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotModelRanking(lm_au, rf_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @import gridExtra
#' @importFrom grDevices blues9
#' @importFrom grid grobTree
#' @import scales
#'
#' @export


plotModelRanking <- function(object, ..., scores = c("MAE", "MSE", "REC", "RROC"),
                             new.score = NULL, table = TRUE){
  if(!("modelPerformance" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelPerformance().")
  if(!("modelPerformance" %in% class(object))) object <- modelPerformance(object, scores, new.score)
  name <- score <- label <- x2 <- y2 <- label2 <- NULL

  df <- object


  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) resp <- modelPerformance(resp, scores, new.score)
      if( "modelPerformance" %in% class(resp)) df <- rbind(df, resp)
    }
  }


  df_scaled <- scaleModelRankingDF(df)
  df_scaled  <- df_scaled [order(df_scaled $name),]
  df_labels <- data.frame(x2 = as.factor(rep(df_scaled$name[1], 5)),
    y2 = c(0.01, 0.25, 0.50, 0.75, 1),
    label2 = as.factor(c(0, 0.25, 0.50, 0.75, 1)))

 p <- ggplot(df_scaled , aes(x = name, y = score)) +
      geom_polygon(aes(group = label, color = label), fill = NA) +
      geom_line(aes(group = label, color = label)) +
      geom_text(data = df_labels, aes(x = x2, y = y2, label = label2)) +
      coord_polar() +
      coord_radar(nNames = length(unique(df_scaled$name))) +
      scale_y_continuous(expand = c(0,0), limits = c(0.01,1)) +
      theme_light() +
      xlab("") +
      ylab("") +
      ggtitle("Model Ranking Radar") +
      theme(panel.border = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())


 if(table ==TRUE){
   df <- df[order(df$name, df$label), ]

   scr <- by(df$score, df$name, function(x){x[1] / x})
   scr <- unlist(scr)
   df$scaled <- scr
   df$scaled <- format(as.numeric(df$scaled), scientific = FALSE, digits = 3)
   df$score <- format(df$score, scientific = TRUE, digits = 3)
   df <- df[ ,c(3,2,1,4)]
   colnames(df)[which(colnames(df) == "score")] <- "values"

   table_score <- tableGrob(df,
    theme=ttheme_minimal(
      core=list(bg_params = list(
      fill = blues9[rep(1:length(unique(df$name)), each = length(unique(df$label)))], col=NA))
      ),
    rows=NULL)

   return(grid.arrange(p, table_score, ncol = 2, widths = c(2,1)))
 }

 return(p)

}


scaleModelRankingDF <- function(df){
  newDF <- data.frame()
  scores <- unique(df$name)
  for(i in scores){
    scoresDF <- df[which(df$name == i),]
    if (!(i %in% c("ROC"))) {
      scoresDF$name <- paste("inv", scoresDF$name)
      minScore <- min(scoresDF$score)
      scoresDF$score <- 1 / scoresDF$score
      scoresDF$score <- scoresDF$score * minScore
    }
    newDF <- rbind(newDF, scoresDF)
  }
  newDF
}


# Modified solution from https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co/37277609

coord_radar <- function(nNames){

  rename_data <- function(coord, data) {
    names(data)[which(colnames(data) == "y")] <- "r"
    names(data)[which(colnames(data) == "x")] <- "theta"
    data
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }

  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }

  render_bg_function <- function(self, scale_details, theme) {
    scale_details <- rename_data(self, scale_details)

    theta <- if (length(scale_details$theta.major) > 0)
      theta_rescale(self, scale_details$theta.major, scale_details)
    thetamin <- if (length(scale_details$theta.minor) > 0)
      theta_rescale(self, scale_details$theta.minor, scale_details)
    thetafine <- seq(0, 2 * pi, length.out = 100)

    rfine <- c(r_rescale(self, scale_details$r.major, scale_details))

    majortheta <- paste("panel.grid.major.", self$theta, sep = "")
    minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
    majorr     <- paste("panel.grid.major.", self$r,     sep = "")

    ggname <- get("ggname", envir = asNamespace("ggplot2"), inherits = FALSE)
    element_render <- get("element_render", envir = asNamespace("ggplot2"), inherits = FALSE)

    ggname("grill", grid::grobTree(
      element_render(theme, "panel.background"),
      if (length(theta) > 0) element_render(
        theme, majortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
        y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
        id.lengths = rep(2, length(theta)),
        default.units = "native"
      ),
      if (length(thetamin) > 0) element_render(
        theme, minortheta, name = "angle",
        x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
        y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
        id.lengths = rep(2, length(thetamin)),
        default.units = "native"
      ),

      element_render(
        theme, majorr, name = "radius",
        x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
        y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
        id.lengths = rep(length(thetafine), length(rfine)),
        default.units = "native"
      )
    ))
  }

  ggproto("CordRadar", CoordPolar, theta = "x", r = "y", start = - pi / nNames,
          direction = 1,
          is_linear = function() TRUE,
          render_bg = render_bg_function)
}
