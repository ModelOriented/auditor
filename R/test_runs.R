#' @title Runs Test
#'
#' @description Note that this test is not very strong. It utilizes only signs of the residuals.
#'
#' @param object model or ordered vector of residuals
#' @param variable name of variable - the observations in the model are ordered by the values of this variable. If NULL - first variable is taken.
#'
#' @importFrom tseries runs.test
#' @importFrom broom augment
#' @importFrom dplyr arrange
#'
#' @export

test_runs <- function(object, variable = NULL){

  if(class(object) == "numeric"){
    res <- object
  } else {
    res <- ordered_model(object, variable)$residuals
    }

  resf <- factor(sign(res))
  RUNS <- runs.test(resf)

  res <- list(
    statistic = RUNS$statistic[[1]],
    p.value = RUNS$p.value,
    name = "Runs test",
    variable = variable
  )

  return(res)
}

