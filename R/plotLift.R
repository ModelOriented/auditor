#' @title Lift Chart
#'
#' @description Lift Chart shows the ratio of a model to a random guess.
#'
#' @param object An object of class ModelAudit
#' @param newdata optionally, a data frame in which to look for variables with which to plot CGains curve. If omitted, the data used to build model will be used.
#' @param newy optionally, required if newdata used. Response vector for new data.
#' @param groups number of groups
#' @param cumulative boolean. If TRUE cumulative lift curve will be plotted.
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' library(auditor)
#' library(mlbench)
#' library(randomForest)
#' data("PimaIndiansDiabetes")
#'
#' model_rf <- randomForest(diabetes~., data=PimaIndiansDiabetes)
#' au_rf <- audit(model_rf, label="rf")
#' plotLIFT(au_rf)
#'
#' model_glm <- glm(diabetes~., family=binomial,	data=PimaIndiansDiabetes)
#' au_glm <- audit(model_glm)
#' plotLIFT(au_rf, au_glm)
#'
#' @export


plotLIFT <- function(object, ..., newdata = NULL, newy, groups = 10, cumulative = TRUE){
  if(class(object)!="modelAudit") stop("plotCGains requires object class modelAudit.")
  depth <- lift <- label <- NULL
  df <- getLIFTDF(object, newdata, newy, groups, cumulative)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getLIFTDF(resp, newdata, newy, groups, cumulative) )
      }
    }
  }

  ggplot(df, aes(x = depth, y = lift, color = label)) +
    geom_line() +
    xlab("Percentage of observations") +
    ylab("Lift") +
    theme_light()
}

getLIFTDF <- function(object, newdata, newy, n.groups, cumulative = TRUE){
  pred <- NULL
  if (is.null(newdata)) {
    predictions <- object$fitted.values
    y <- as.numeric(as.character(object$y))
  } else {
    if(is.null(newy)) stop("newy must be provided.")
    predictions <- object$predict.function(object$model, newdata)
    y <- as.numeric(as.character(newy))
  }
  df <- data.frame(pred=predictions, y=y)
  df <- arrange(df, desc(pred))

  group <- ceiling(seq_along(df[,2])/floor(nrow(df)/n.groups))

  cap <- floor(nrow(df)/n.groups) * n.groups
  df <-  stats::aggregate(df[1:cap,2], by=list(group[1:cap]), mean)

  if (cumulative==TRUE) {
    df[,2] <- cumsum(df[,2])/seq_along(df[,2])
  }
  colnames(df) <- c("depth", "lift")
  df$lift <- df$lift/mean(y)
  df$depth <- 100* df$depth / n.groups
  df$label <- object$label
  return(df)
}

