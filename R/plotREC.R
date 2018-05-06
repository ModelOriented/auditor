#' @title Regression Error Characteristic Curves (REC)
#'
#' @description Error Characteristic curves are a generalization of ROC curves.
#' On the x axis of the plot there is an error tolerance and on the y axis there is a percentage of observations predicted within the given tolerance.
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
#' @seealso \code{\link{plot.modelAudit}, \link{plotROC}, \link{plotRROC}}
#'
#' @examples
#' library(auditor)
#' library(randomForest)
#' library(car)
#' model_lm <- lm(prestige ~ education + women + income, data = Prestige)
#' audit_lm <- audit(model_lm)
#'
#' plotREC(audit_lm)
#'
#' model_rf <- randomForest(prestige ~ education + women + income, data = Prestige)
#' audit_rf <- audit(model_rf)
#' plotREC(audit_lm, audit_rf)
#'
#'
#' @export


plotREC <- function(object, ...){
  RECX <- RECY <- RECX0 <- RECY0 <- label <- NULL
  df <- getRECDF(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit"){
        df <- rbind( df, getRECDF(resp) )
      }
    }
  }

  ggplot(df, aes(x=RECX, y=RECY, color = label)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0,1,0.1),
                       labels = paste(seq(0, 100, 10),"%"),
                       name = "Accuracy") +
    theme_light() +
    xlab("Error tolerance") +
    ggtitle("REC Curve")

}

getRECDF <- function(object){
  err <- sort(abs(object$fitted.values - object$y))
  err <- c(0, err)
  n <- length(err)
  RECX <- numeric(n)
  RECY <- numeric(n)
  RECX[1] <- RECY[1] <- correct <- absDev <- 0
  for(i in 2:n){
    if (err[i] > err[i-1]) {
      absDev <- correct/n
    }
    RECX[i] <-  err[i]
    RECY[i] <- absDev
    correct <- correct + 1
  }

  df <- data.frame(RECX = RECX, RECY = RECY, label = object$label)
  return(df)
}
