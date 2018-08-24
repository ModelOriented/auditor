#' @title Regression Error Characteristic Curves (REC)
#'
#' @description Error Characteristic curves are a generalization of ROC curves.
#' On the x axis of the plot there is an error tolerance and on the y axis there is a percentage of observations predicted within the given tolerance.
#'
#' @param object An object of class ModelAudit or modelResiduals.
#' @param ... Other modelAudit or model Residuals objects to be plotted together.
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
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotREC(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotREC(lm_au, rf_au)
#'
#'
#' @export


plotREC <- function(object, ...){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object)
  RECX <- RECY <- RECX0 <- RECY0 <- label <- NULL

  df <- getRECDF(object)

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) df <- rbind( df, getRECDF(modelResiduals(resp)) )
      if("modelResiduals" %in% class(resp)) df <- rbind(df, getRECDF(resp))
    }
  }

  ggplot(df, aes(x=RECX, y=RECY, color = label)) +
    geom_line() +
    scale_y_continuous(breaks = seq(0,1,0.1),
                       labels = paste(seq(0, 100, 10),"%")) +
    theme_light() +
    xlab("error tolerance") +
    ylab("") +
    ggtitle("REC Curve")

}

getRECDF <- function(object){
  err <- sort(abs(object$res))
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

  df <- data.frame(RECX = RECX, RECY = RECY, label = object$label[1])
  return(df)
}
