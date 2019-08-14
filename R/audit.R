#' @title Create modelAudit object
#'
#' @description Function \code{audit} create modelAudit object for further validation of a model.
#' Models may have very different structures. This function creates a unified representation of a model and calculates residuals,
#' which can be further processed by various error analysis functions.
#'
#' Function 'audit()' is deprecated, please, use an object of class 'explainer' created with function
#' \code{\link[DALEX]{explain}} from the DALEX package.
#'
#' @param object An object containing a model or object of class explainer (see \code{\link[DALEX]{explain}}).
#' @param data Data.frame or matrix - data that will be used by further validation functions. If not provided, will be extracted from the model.
#' @param y Response vector that will be used by further validation functions. Some functions may require an integer vector containing binary labels with values 0,1.  If not provided, will be extracted from the model.
#' @param predict.function Function that takes two arguments: model and data. It should return a numeric vector with predictions.
#' @param residual.function Function that takes three arguments: model, data and response vector. It should return a numeric vector with model residuals for given data. If not provided, response residuals (\eqn{y-\hat{y}}) are calculated.
#' @param label Character - the name of the model. By default it's extracted from the 'class' attribute of the model.
#'
#' @return An object of class ModelAudit, which contains:
#' #' \itemize{
#' \item \code{model.class} class of the audited model,
#' \item \code{label} the name of the model,
#' \item \code{model} the audited model,
#' \item \code{fitted.values} fitted values from model,
#' \item \code{data} data used for fitting the model,
#' \item \code{y} vector with values of predicted variable used for fitting the model,
#' \item \code{predict.function} function that were used for model predictions,
#' \item \code{residual.function} function that were used for calculating model residuals,
#' \item \code{residuals}
#' \item \code{std.residuals} standardized residuals - the residuals divided by theirs standard deviation.
#' }
#'
#'
#' @importFrom stats model.frame sd
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm)
#'
#' p_fun <- function(model, data){predict(model, data, response = "link")}
#' audit_glm_newpred <- audit(model_glm, predict.function = p_fun)
#'
#'
#' library(randomForest)
#' model_rf <- randomForest(Species ~ ., data=iris)
#' audit_rf <- audit(model_rf)
#'
#' @importFrom DALEX explain
#'
#' @export

audit <- function(object, data=NULL, y = NULL, predict.function = NULL, residual.function = NULL, label=NULL){
  UseMethod("audit")
}

#' @export
audit.default <- function(object, data=NULL, y = NULL, predict.function = NULL, residual.function = NULL, label=NULL){

  result <- explain(object, data = data, y = y, predict_function = predict.function,
                    label = label, residual_function = residual.function)

  return(result)
}

#' @export
audit.explainer <- function(object, data=NULL, y = NULL, predict.function = NULL, residual.function = NULL, label=NULL){
  message("Please note that 'audit()' is now deprecated, it is better to use 'explain()' form the 'DALEX' instead.")
  return(object)
}
