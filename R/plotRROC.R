#' @title Regression Receiver Operating Characteristic (RROC)
#'
#' @description The basic idea of the ROC curves for regression is to show model asymmetry.
#' The RROC is a plot where on the x-axis we depict total over-estimation and on the y-axis total
#' under-estimation.
#'
#' @param object An object of class modelAudit or modelResiduals.
#' @param ... Other modelAudit or model Residuals objects to be plotted together.
#'
#' @return ggplot object
#'
#' @details For RROC curves we use a shift, which is an equivalent to the threshold for ROC curves.
#' For each observation we calculate new prediction: \eqn{\hat{y}'=\hat{y}+s} where s is the shift.
#' Therefore, there are different error values for each shift: \eqn{e_i = \hat{y_i}' - y_i}
#'
#' Over-estimation is calculated as: \eqn{OVER= \sum(e_i|e_i>0)}.
#'
#' Under-estimation is calculated as: \eqn{UNDER = \sum(e_i|e_i<0)}.
#'
#'  The shift equals 0 is represented by a dot.
#'
#'  The Area Over the RROC Curve (AOC) equals to the variance of the errors multiplied by \eqn{frac{n^2}{2}}.
#'
#' @references Hernández-Orallo, José. 2013. ‘ROC Curves for Regression’. Pattern Recognition 46 (12): 3395–3411.
#'
#' @seealso \code{\link{plot.modelAudit}, \link{plotROC}, \link{plotREC}}
#'
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotRROC(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(prestige~education + women + income, data = Prestige)
#' rf_au <- audit(rf_model, data = Prestige, y = Prestige$prestige)
#' plotRROC(lm_au, rf_au)
#'
#' @import ggplot2
#'
#' @export


plotRROC <- function(object, ...){
  if(!("modelResiduals" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelResiduals().")
  if(!("modelResiduals" %in% class(object))) object <- modelResiduals(object)
  RROCX <- RROCY <- RROCX0 <- RROCY0 <- label <- NULL

  df <- getRROCDF(object)

  err <- sort(object$fitted.values - object$y)
  RROCX0 <- sum(err[which(err > 0)], na.rm = TRUE )
  RROCY0 <- sum(err[which(err < 0)], na.rm = TRUE )
  df0 <- data.frame(RROCX0 = RROCX0, RROCY0 = RROCY0, label = object$label)


  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if(class(resp)=="modelAudit" || class(resp)=="modelResiduals"){
        if("modelAudit" %in% class(resp)) resp <- modelResiduals(resp)
        df <- rbind( df, getRROCDF(resp) )
        err <- sort(resp$fitted.values - resp$y)
        RROCX0 <- sum(err[which(err > 0)], na.rm = TRUE )
        RROCY0 <- sum(err[which(err < 0)], na.rm = TRUE )
        df0 <- rbind(df0, data.frame(RROCX0 = RROCX0, RROCY0 = RROCY0, label=resp$label[1]))
      }
    }
  }




  ggplot(df, aes(x=RROCX, y=RROCY, color = label)) +
    geom_line() +
    geom_point(data = df0, aes(x=RROCX0, y=RROCY0), size = 3) +
    theme_light() +
    ylab("under-estimation") +
    xlab("over-estimation") +
    ggtitle("RROC Curve")

}


getRROCDF <- function(object){
  err <- sort(object$fitted.values - object$y)
  n <- length(err)
  RROCX <- numeric(n+2)
  RROCY <- numeric(n+2)
  RROCX[1] <- 0
  RROCY[1] <- -Inf

  for(i in 1:n){
    s <- -err[i]
    tErr <- err + s
    RROCX[i+1] <- sum(tErr[which(tErr > 0)], na.rm = TRUE )
    RROCY[i+1] <- sum(tErr[which(tErr < 0)], na.rm = TRUE )
  }

  RROCX[n+2] <- Inf
  RROCY[n+2] <- 0

  df <- data.frame(RROCX = RROCX, RROCY = RROCY, label = object$label[1])
  return(df)
}



