#' @title Durbin-Watson Score
#'
#' @description Score based on Durbin-Watson test statistic.
#' The score value is helpful in comparing models. It is worth ponting out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#'
#' @param object object An object of class ModelAudit
#' @param variable "Fitted values" or name of dependent or independent variable to order residuals. If NULL the original data order is taken.
#'
#' @importFrom car durbinWatsonTest
#'
#' @return an object of class scoreAudit
#'
#' @export

scoreDW <- function(object, variable = NULL){
  if(is.null(variable) || variable=="Fitted values") {
    dataDW <- data.frame(variable=object$fitted.values, residuals = object$residuals)
  } else {
    dataDW <- data.frame(variable=object$data[,variable], residuals = object$residuals)
  }

  if(!is.null(variable)){
    dataDW <- dplyr::arrange(dataDW, variable)
  }

    residuals <- as.vector(dataDW$residuals)

  result <- list(
    name = "Durbin-Watson",
    score = durbinWatsonTest(residuals)
  )
  class(result) <- "scoreAudit"
  return(result)
}



