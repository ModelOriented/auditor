#' @title auditor
#'
#' @description Function \code{auditor} validates a regression model.
#'
#' @param model An object of appropriate class containing a model.
#' @param variable name of variable - for some tests the observations will be ordered by the values of this variable. If NULL - first variable is taken.
#'
#' @return An object of class ModelAudit.
#'
#' @importFrom broom augment
#' @importFrom stats model.frame pf
#'
#' @export


auditor <- function(model, variable = NULL){
  model.data <- model.frame(model)
  broom.aug <- augment(model)

  if (is.null(variable))  variable <- colnames(model.data)[2]
  ordered.resid <- arrange_(broom.aug, variable)$.resid

  tests <- list(
    model = model,
    variable = variable,
    residuals = broom.aug$.resid,
    std.residuals = broom.aug$.std.resid,
    ordered.resid = ordered.resid,
    cooks.dist = setNames(broom.aug$.cooksd, c(1:nrow(broom.aug))),
    hat.values = setNames(broom.aug$.hat, c(1:nrow(broom.aug))),
    data = model.data,
    gqtest = c(name = "Goldfeld-Quandt", assumption = "Homoscedasticity of residuals", test_gq(model, variable)),
    dwtest = c(name = "Durbin-Watson", assumption = "Autocorrelation of residuals", test_dw(ordered.resid)),
    runstest = c(name = "Runs", assumption = "Autocorrelation of residuals", test_runs(ordered.resid))
  )
  class(tests) <- "ModelAudit"
  return(tests)
}

