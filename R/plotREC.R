#' @title Regression Error Characteristic Curves (REC)
#'
#' @description Error Characteristic curves are a generalization of ROC curves.
#' On the x axis of the plot there is an error tolerance and on the y axis there is a percentage of observations predicted within the given tolerance.
#' The REC curve estimates the cumulative distribution function (CDF) of the error.
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @details REC curve estimates the Cumulative Distribution Function (CDF) of the error
#'
#' Area Over the REC Curve (REC) is a biased estimate of the expected error
#'
#' @references Bi J., Bennett K.P. (2003). Regression error characteristic curves, in: Twentieth International Conference on Machine Learning (ICML-2003), Washington, DC.
#'
#' @import ggplot2
#'
#' @export


plotREC <- function(object, ...){
  RECX <- RECY <- RECX0 <- RECY0 <- label <- NULL
  df <- getRECDF(object)
  df <- data.frame(df, label = object$label)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, data.frame(getRECDF(resp), label = resp$label) )
      }
    }
  }

  ggplot(df, aes(x=RECX, y=RECY, color = label)) +
    geom_line() +
    theme_light() +
    ylab("Accuracy") +
    xlab("Absolute deviation")



}

getRECDF <- function(object){
  err <- sort(abs(object$fitted.values - object$y))
  err <- c(0, err)
  n <- length(err)
  RECX <- numeric(n)
  RECY <- numeric(n)
  RECX[1] <- -Inf
  RECY[1] <- 0
  correct <- 0
  absDev <- 0
  for(i in 2:n){
    if (err[i] > err[i-1]) {
      absDev <- correct/n
    }
    RECX[i] <-  err[i]
    RECY[i] <- absDev
    correct <- correct + 1
  }

  RECX[n] <- Inf
  RECY[n] <- Inf

  df <- data.frame(RECX = RECX, RECY = RECY)
  return(df)
}
