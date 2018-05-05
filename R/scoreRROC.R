#' @title Area Over the Curve for RROC Curves
#'
#' @description The area over the Regression Receiver Operating Characteristic
#'
#' @param object An object of class ModelAudit
#'
#' @return numeric
#'
#' @seealso \code{\link{plotRROC}}
#'
#' @references Hernández-Orallo, José. 2013. ‘ROC Curves for Regression’. Pattern Recognition 46 (12): 3395–3411.
#'
#' @export


scoreRROC <- function(object){

  RROCF <- getRROCDF(object)
  x <- RROCF$RROCX
  y <- RROCF$RROCY

  aoc <- 0
  for (i in 2:(length(x) - 2)) {
    print(0.5 * (y[i+1] + y[i]) * (x[i+1] - x[i]))
    aoc <- aoc + 0.5 * (y[i+1] + y[i]) * (x[i+1] - x[i])
  }
  aoc

}

# getRROCDF is in plotRROC.R file



