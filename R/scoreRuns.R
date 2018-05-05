#' @title Runs Score
#'
#' @description Score based on Runs test statistic. Note that this test is not very strong. It utilizes only signs of the residuals.
#' The score value is helpful in comparing models. It is worth ponting out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#'
#' @param object object An object of class ModelAudit
#' @param variable "Fitted values" or name of dependent or independent variable to order residuals. If NULL original data order is taken.
#'
#' @importFrom tseries runs.test
#'
#' @export

scoreRuns <- function(object, variable = NULL){
  if(is.null(variable) || variable=="Fitted values") {
    dataRuns <- data.frame(variable=object$fitted.values, residuals = object$residuals)
  } else {
    dataRuns <- data.frame(variable=object$data[,variable], residuals = object$residuals)
  }
    if(!is.null(variable)){
      dataRuns <- dplyr::arrange(dataRuns, variable)
    }
    residuals <- dataRuns$residuals
    signumOfResiduals <- factor(sign(residuals))
    RunsTested <- runs.test(signumOfResiduals)


  result <- list(
    name = "Runs",
    score = RunsTested$statistic[[1]],
    pValue = RunsTested$p.value
  )

    class(result) <- "scoreAudit"
  return(result)
}



