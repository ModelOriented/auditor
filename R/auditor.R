#' @title auditor
#'
#' @description Function \code{auditor} validates a regression model.
#'
#' @param model An object of appropriate class containing a model.
#' @param vars vector of variables - for some tests and plots observations in the model are ordered by the values of this variables. If NULL - all variables are taken.
#'
#' @return An object of class ModelAudit.
#'
#' @importFrom broom augment
#' @importFrom stats model.frame pf
#' @importFrom car vif
#' @importFrom stats lm setNames
#'
#' @export



auditor <- function(model, vars = NULL){
  UseMethod("auditor")
}

auditor.default <- function(model, vars = NULL) {
  stop("augment doesn't know how to deal with data of class ", class(model), call. = FALSE)
}

#' @export
auditor.lm <- function(model, vars = NULL){
  dataFromModel <- model.frame(model)
  augmentModel <- augment(model)
  if (is.null(vars))  vars <- colnames(dataFromModel)[-1]
  result <- list(
    model = model,
    data = dataFromModel,
    variables = vars,
    residuals = augmentModel$.resid,
    std.residuals = augmentModel$.std.resid,
    cooks.dist = setNames(augmentModel$.cooksd, c(1:nrow(augmentModel))),
    hat.values = setNames(augmentModel$.hat, c(1:nrow(augmentModel))),
    VIF = vif(lm(dataFromModel)),
    testGQ = c(name = "Goldfeld-Quandt", assumption = "Homoscedasticity of residuals", testGQ(model, vars)),
    dwtest = c(name = "Durbin-Watson", assumption = "Autocorrelation of residuals", testDW(model, vars)),
    testRuns = c(name = "Runs", assumption = "Autocorrelation of residuals", testRuns(model, vars))
  )
  class(result) <- "modelAudit"
  return(result)
}

#' @export
auditor.randomForest <- function(model, vars = NULL){
  dataFromModel <- model.frame(model)
  if (is.null(vars))  vars <- colnames(dataFromModel)[-1]
  result <- list(
    model = model,
    data = dataFromModel,
    variables = vars,
    residuals = getResiduals(model, dataFromModel[, 1]),
    std.residuals = getStdResiduals(model, dataFromModel[, 1]),
    VIF = vif(lm(dataFromModel)),
    testGQ = c(name = "Goldfeld-Quandt", assumption = "Homoscedasticity of residuals", testGQ(model, vars)),
    dwtest = c(name = "Durbin-Watson", assumption = "Autocorrelation of residuals", testDW(model, vars)),
    testRuns = c(name = "Runs", assumption = "Autocorrelation of residuals", testRuns(model, vars))
  )
  class(result) <- "modelAudit"
  return(result)
}
