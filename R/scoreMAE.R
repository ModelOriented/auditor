#' @title Mean Absolute Error
#'
#' @description Mean Absolute Error.
#'
#' @param object An object of class ModelAudit.
#'
#' @return an object of class scoreAudit
#'
#' @seealso \code{\link{score}}
#'
#' @export


scoreMAE <- function(object){


  MAEResults <- list(
    name = "MAE",
    score = mean(abs(object$y - object$fitted.values))
    )

  class(MAEResults) <- "scoreAudit"
  return(MAEResults)
}



