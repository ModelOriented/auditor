#' @title auditor
#'
#' @description Function \code{auditor} validates a regression model.
#'
#' @param model An object of appropriate class containing a model.
#' @param ... further arguments passed to or from other methods.
#'
#' @return An object of class ModelAudit.
#'
#' @importFrom lmtest bptest
#' @importFrom lmtest gqtest
#' @importFrom lmtest hmctest
#' @importFrom broom augment
#'
#' @export


auditor <- function(model, ...){
  broom.aug <- augment(model)
  tests <- list(
    model = model,
    residuals = broom.aug$.resid,
    std.residuals = broom.aug$.std.resid,
    data = model.frame(model),
    gqtest = c(name = "Goldfeld-Quandt", assumption = "Homoscedasticity of residuals", test_gq(model, ...))
  )
  class(tests) <- "ModelAudit"
  return(tests)
}

