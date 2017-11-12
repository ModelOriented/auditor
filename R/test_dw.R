#' @title Durbin-Watson Test
#'
#' @description Test for autocorrelation with the Durbin-Watson Test.
#'
#' @param object model or ordered vector of residuals
#' @param variable name of variable - the observations in the model are ordered by the values of this variable. If NULL - first variable is taken.
#'
#' @importFrom car durbinWatsonTest
#'
#' @export

test_dw <- function(object, variable = NULL){

  if(class(object) == "numeric"){
    res <- object
  } else {
    res <- ordered_model(object, variable)$residuals
  }

  DW <- durbinWatsonTest(res)

  res <- list(
    statistic = DW,
    name = "Durbin-Watson",
    residuals = res,
    p.value = ""
  )

  return(res)
}

