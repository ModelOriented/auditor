#' @title Regression Receiver Operating Characteristic (RROC)
#'
#' @description The basic idea of the ROC curves for regression is to show model asymmetry.
#' The RROC is a plot where on the x-axis we depict total over-estimation and on the y-axis total
#' under-estimation.
#'
#' @param object An object of class ModelAudit
#' @param ... other modelAudit objects to be plotted together
#'
#' @return ggplot object
#'
#' @details For RROC curves we use a shift, which is an equvalent to the threshold for ROC curves.
#' For each observation we calculate new prediction: \eqn{\hat{y}'=\hat{y}+s}
#'  where s is the shift.
#' \eqn{OVER= \sum(e_i|e_i>0)}
#'  \eqn{UNDER = \sum(e_i|e_i<0)}
#'  The shift equals 0 is represented by dot.
#'
#' @references Hernández-Orallo, J. (2013). ROC curves for regression. Pattern Recognition, 46, 3395-3411.
#'
#' @import ggplot2
#'
#' @export


plotROCR <- function(object, ...){
  RROCX <- RROCY <- RROCX0 <- RROCY0 <- label <- NULL
  df <- getRROCDF(object)
  df <- data.frame(df, label = class(object$model)[1])
  err <- sort(object$fitted.values - object$y)
  RROCX0 <- sum(err[which(err > 0)], na.rm = TRUE )
  RROCY0 <- sum(err[which(err < 0)], na.rm = TRUE )
  df0 <- data.frame(RROCX0 = RROCX0, RROCY0 = RROCY0, label = object$label)


  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      df <- rbind( df, data.frame(getRROCDF(resp), label = resp$label) )
      err <- sort(resp$fitted.values - resp$y)
      RROCX0 <- sum(err[which(err > 0)], na.rm = TRUE )
      RROCY0 <- sum(err[which(err < 0)], na.rm = TRUE )
      df0 <- rbind(df0, data.frame(RROCX0 = RROCX0, RROCY0 = RROCY0, label=resp$label))
    }
  }




  ggplot(df, aes(x=RROCX, y=RROCY, color = label)) +
    geom_line() +
    geom_point(data = df0, aes(x=RROCX0, y=RROCY0), size = 3) +
    theme_light() +
    ylab("UNDER") +
    xlab("OVER")

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

  df <- data.frame(RROCX = RROCX, RROCY = RROCY)
  return(df)
}



