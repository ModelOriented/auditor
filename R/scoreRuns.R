#' @title Runs Score
#'
#' @description Score based on Runs test statistic. Note that this test is not very strong. It utilizes only signs of the residuals.
#' The score value is helpful in comparing models. It is worth ponting out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#'
#' @param object object An object of class ModelAudit.
#' @param variable name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @importFrom tseries runs.test
#'
#' @export

scoreRuns <- function(object, variable = NULL){

  orderedResiduals <- orderResidualsDF(object, variable)

  signumOfResiduals <- factor(sign(orderedResiduals))
  RunsTested <- runs.test(signumOfResiduals)


  result <- list(
    name = "Runs",
    score = RunsTested$statistic[[1]],
    pValue = RunsTested$p.value
  )

    class(result) <- "scoreAudit"
  return(result)
}



