#' @title Model Ranking Plot
#'
#' @description Radar plot with model scores. Scores are scaled to [0,1], each score is inversed and divided by maximum score value.
#'
#' @param object An object of class ModelAudit.
#' @param ... Other modelAudit objects to be plotted together.
#' @param type Vector of score names to be plotted.
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
#'
#' @export


plotModelRanking <- function(object, ..., type = c("MAE", "MSE", "REC", "RROC"),
                             new.score = NULL, table = TRUE){
  if(!("modelPerformance" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelPerformance().")
  if(!("modelPerformance" %in% class(object))) object <- modelPerformance(object, type, new.score)
  name <- score <- label <- NULL

  df <- object


  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) resp <- modelPerformance(resp, type, new.score)
      if( "modelPerformance" %in% class(resp)) {
        df <- rbind(df, resp)
      }
    }
  }

  df_scaled <- scaleModelRankingDF(df)
  df_scaled  <- df_scaled [order(df_scaled $name),]

 p <- ggplot(df_scaled , aes(x = name, y = score, group = label, color = label)) +
        geom_polygon(fill = NA) +
        ylim(0,1) +
        geom_line() +
        coord_polar() +
        coord_radar() +
        theme_light() +
        xlab("") +
        ylab("") +
        ggtitle("Model Ranking Radar")

 if(table ==TRUE){
   df <- df[order(df$name, df$label), ]

     scr <- by(df$score, df$name, function(x){x / x[1]})
     scr <- unlist(scr)
     df$scaled <- scr
     df$scaled <- format(as.numeric(df$scaled), scientific = FALSE, digits = 3)
     df$score <- format(df$score, scientific = TRUE, digits = 3)
     df <- df[ ,c(3,2,4,1)]


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
  type <- unique(df$name)
  for(i in type){
    typeDF <- df[which(df$name == i),]
    if (!(i %in% c("ROC"))) {
      typeDF$name <- paste("inv", typeDF$name)
      typeDF$score <- 1 / typeDF$score
      maxScore <- max(typeDF$score)
      typeDF$score <- typeDF$score / maxScore
    }
    newDF <- rbind(newDF, typeDF)
  }
  newDF
}


coord_radar <- function(){
  ggproto(NULL, CoordPolar,
    theta='x', r='y',
    start=0, direction=1,
    is_linear=function() TRUE)
}
