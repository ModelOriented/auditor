#' @title LIFT Chart
#'
#' @description LIFT Chart shows the ratio of a model to a random guess.
#'
#' @param object An object of class ModelAudit.
#' @param groups Number of groups.
#' @param cumulative If TRUE cumulative lift curve will be plotted.
#' @param ... Other modelAudit objects to be plotted together.
#'
#' @details Response vector provided by y argument in audit function should be an integer vector containing binary labels with values 0,1.
#'
#' @return ggplot object
#'
#' @examples
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#' glm_model <- glm(diabetes~., family=binomial,	data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes)
#' plotLIFT(glm_au)
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @importFrom stats aggregate
#'
#' @export


plotLIFT <- function(object, ..., groups = 10, cumulative = TRUE){
  if (class(object)!="modelAudit") stop("plotCGains requires object class modelAudit.")
  if (!(unique(object$y) == c(0,1) || unique(object$y)==c(1,0))) stop("Response vector y should be an integer vector containing binary labels with values 0,1.")

  depth <- lift <- label <- NULL
  df <- getLIFTDF(object, groups, cumulative)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getLIFTDF(resp, groups, cumulative) )
      }
    }
  }

  ggplot(df, aes(x = depth, y = lift, color = label)) +
    geom_line() +
    xlab("percentage of observations") +
    ylab("LIFT") +
    ggtitle("LIFT Chart") +
    theme_light()
}

getLIFTDF <- function(object, n.groups, cumulative = TRUE){
  pred <- NULL
  y = as.numeric(as.character(object$y))
  df <- data.frame(pred=object$fitted.values, y=y)
  df <- df[order(-df$pred),]

  group <- ceiling(seq_along(df[,2])/floor(nrow(df)/n.groups))

  cap <- floor(nrow(df)/n.groups) * n.groups
  df <-  aggregate(df[1:cap,2], by=list(group[1:cap]), mean)

  if (cumulative==TRUE) {
    df[,2] <- cumsum(df[,2])/seq_along(df[,2])
  }
  colnames(df) <- c("depth", "lift")
  df$lift <- df$lift/mean(y)
  df$depth <- 100* df$depth / n.groups
  df$label <- object$label
  return(df)
}

