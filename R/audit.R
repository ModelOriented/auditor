#' @title Deprecated
#'
#' @description
#' The \code{audit()} function is deprecated, use \code{\link[DALEX]{explain}} from the \code{DALEX} package instead.
#'
#' @param object An object containing a model or object of class explainer (see \code{\link[DALEX]{explain}}).
#' @param data Data.frame or matrix - data that will be used by further validation functions. If not provided, will be extracted from the model.
#' @param y Response vector that will be used by further validation functions. Some functions may require an integer vector containing binary labels with values 0,1.  If not provided, will be extracted from the model.
#' @param predict.function Function that takes two arguments: model and data. It should return a numeric vector with predictions.
#' @param residual.function Function that takes three arguments: model, data and response vector. It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated.
#' @param label Character - the name of the model. By default it's extracted from the 'class' attribute of the model.
#' @param predict_function Function that takes two arguments: model and data. It should return a numeric vector with predictions.
#' @param residual_function Function that takes three arguments: model, data and response vector. It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated.

#'
#' @return An object of class \code{explainer}.
#'
#' @importFrom stats model.frame sd
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#' audit_glm <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' p_fun <- function(model, data) { predict(model, data, response = "link") }
#' audit_glm_newpred <- audit(model_glm,
#'                            data = titanic_imputed,
#'                            y = titanic_imputed$survived,
#'                            predict.function = p_fun)
#'
#'
#' library(randomForest)
#' model_rf <- randomForest(Species ~ ., data=iris)
#' audit_rf <- audit(model_rf)
#'
#' @importFrom DALEX explain
#'
#' @export
audit <- function(object, data=NULL, y = NULL, predict.function = NULL, residual.function = NULL, label=NULL,
                  predict_function = NULL, residual_function = NULL){
  UseMethod("audit")
}

#' @export
audit.default <- function(object, data=NULL, y = NULL, predict.function = NULL, residual.function = NULL, label=NULL,
                          predict_function = NULL, residual_function = NULL){
  if(is.null(predict.function))   predict.function <- predict_function
  if(is.null(residual.function)) residual.function <- residual_function
  result <- explain(object, data = data, y = y, predict_function = predict.function,
                    label = label, residual_function = residual.function)

  return(result)
}

#' @export
audit.explainer <- function(object, data=NULL, y = NULL, predict.function = NULL, residual.function = NULL, label=NULL,
                            predict_function = NULL, residual_function = NULL){
  return(object)
}
