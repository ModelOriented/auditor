#' @title Lift Chart
#'
#' @description Lift Chart shows the ratio of a model to a random guess.
#'
#' @param object An object of class ModelAudit
#' @param groups number of groups
#' @param cumulative boolean. If TRUE cumulative lift curve will be plotted.
#' @param ... other modelAudit objects to be plotted together
#'
#' @details Response vector provided by y argument in audit function should be an integer vector containing binary labels with values 0,1.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @import ggplot2
#' @import dplyr
#'
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
    xlab("Percentage of observations") +
    ylab("Lift") +
    theme_light()
}

getLIFTDF <- function(object, n.groups, cumulative = TRUE){
  pred <- NULL
  y = as.numeric(as.character(object$y))
  df <- data.frame(pred=object$fitted.values, y=y)
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

