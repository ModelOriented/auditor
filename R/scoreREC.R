#' @title Area Over the Curve for REC Curves
#'
#' @description The area over the Regression Error Characteristic curve is a measure of the expected error
#' for the regression model.
#' It is calculated as a sample mean of errors.
#'
#' @param object An object of class ModelAudit
#'
#' @return numeric
#'
#' @seealso \code{\link{plotREC}}
#'
#'
#' @export


scoreREC <- function(object){

  RECDF <-auditor:::getRECDF(object)
  x <- RECDF$RECX
  y <- RECDF$RECY

  aoc <- max(x) * max(y)
  for (i in 2:length(x)) {
    aoc <- aoc - 0.5 * (x[i] - x[i - 1]) * (y[i] + y[i - 1])
  }
  aoc

}

# getRECDF is in plotREC.R file


