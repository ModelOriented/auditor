#' @title Runs Score
#'
#' @description Score based on Runs test statistic. Note that this test is not very strong. It utilizes only signs of the residuals.
#' The score value is helpful in comparing models. It is worth pointing out that results of tests like p-value makes sense only
#' when the test assumptions are satisfied. Otherwise test statistic may be considered as a score.
#'
#' @param object An object of class 'explainer' created with function \code{\link[explain]{DALEX}} from the DALEX package.
#' @param variable name of model variable to order residuals. If value is NULL data order is taken. If value is "Predicted response" or "Fitted values" then data is ordered by fitted values. If value is "Observed response" the data is ordered by a vector of actual response (\code{y} parameter passed to the \code{\link{audit}} function).
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' score_runs(lm_au)
#'
#' @export

score_runs <- function(object, variable = NULL){
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  object <- model_residual(object, variable)


  orderedResiduals <- object$res

  sinum_of_res <- factor(sign(orderedResiduals))

  n <- length(sinum_of_res)
  R <- 1 + sum(as.numeric(sinum_of_res[-1] != sinum_of_res[-n]))
  n1 <- sum(levels(sinum_of_res)[1] == sinum_of_res)
  n2 <- sum(levels(sinum_of_res)[2] == sinum_of_res)
  m <- 1 + 2*n1*n2 / (n1+n2)
  s <- sqrt(2*n1*n2 * (2*n1*n2 - n1 - n2) / ((n1+n2)^2 * (n1+n2-1)))

  statistic <- (R - m) / s
  pvalue <- 2 * pnorm(-abs(statistic))


  result <- list(
    name = "Runs",
    score = statistic,
    pValue = pvalue
  )

    class(result) <- "auditor_score"
  result
}


#' @rdname score_runs
#' @export
scoreRuns<- function(object,  variable = NULL) {
  message("Please note that 'scoreRuns()' is now deprecated, it is better to use 'score_runs()' instead.")
  score_runs(object, variable)
}
