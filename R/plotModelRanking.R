#' @title Model Ranking Plot
#'
#' @description Radar plot with model scores. Scores are scaled to [0,1], each score is inversed and divided by maximum score value.
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#' @param type vector of score names to be plotted.
#' @param new.score a function or list of functions that take one argument: object of class ModelAudit and return a numeric value.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#'
#' @export


plotModelRanking <- function(object, ..., type = c("MAE", "MSE", "REC", "RROC"), new.score = NULL){
  name <- score <- label <- NULL


  df <- getModelRankingDF(object, type, new.score)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getModelRankingDF(resp, type, new.score))
      }
    }
  }

  df <- scaleModelRankingDF(df)

  ggplot(df, aes(x = name, y = score, group = label, color = label)) +
    geom_polygon(fill = NA) +
    ylim(0,1) +
    geom_line() +
    coord_polar() +
    coord_radar() +
    theme_light() +
    xlab("") +
    ylab("") +
    ggtitle("Model Ranking Radar")



}


getModelRankingDF <- function(object, type, new.score){

  df <- data.frame(score = score(object, type = type[1])$score, label = object$label, name = type[1])

  if(length(type) > 1){
    for(i in 2:length(type)){
      df <- rbind(df, data.frame(score = score(object, type = type[i])$score, label = object$label, name = type[i]))
    }
  }

  if(!is.null(new.score)){
    if (class(new.score) == "function"){
      df <- rbind(df, data.frame(score = new.score(object, label = object$label, name = as.character(substitute(new.score)))))
    }
    if(class(new.score) == "list") {
      for(i in names(new.score)){
        df <- rbind(df, data.frame(score = new.score[[i]](object), label = object$label, name = i))
      }
    }
  }
  return(df)
}

scaleModelRankingDF <- function(df){
  newDF <- data.frame()
  type <- unique(df$name)
  for(i in type){
    typeDF <- df[which(df$name == i),]
    if (!(i %in% c("ROC"))) {
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
