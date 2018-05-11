#' @title Area Over the Curve for REC Curves
#'
#' @description The area over the Regression Error Characteristic curve is a measure of the expected error
#' for the regression model.
#'
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' scoreREC(lm_au)
#'
#'
#' @seealso \code{\link{plotREC}}
#'
#' @references J. Bi, and K. P. Bennet, "Regression error characteristic curves," in Proc. 20th Int. Conf. Machine Learning, Washington DC, 2003, pp. 43-50
#'
#' @export


scoreREC <- function(object){

  RECDF <- getRECDF(object)
  x <- RECDF$RECX
  y <- RECDF$RECY

  aoc <- max(x) * max(y)
  for (i in 2:length(x)) {
    aoc <- aoc - 0.5 * (x[i] - x[i - 1]) * (y[i] + y[i - 1])
  }

  RECResults <- list(
    name = "REC",
    score = aoc
    )

  class(RECResults) <- "scoreAudit"
  return(RECResults)
}

# getRECDF is in plotREC.R file


