#' @title Create modelAudit object
#'
#' @description Function \code{audit} create modelAudit object for further validation of a model.
#' Models may have very different structures. This function creates a unified representation of a model and calculates residuals,
#' which can be further processed by various error analysis functions.
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
#' @return An object of class model_audit.
#'
#' @importFrom stats model.frame sd
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
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
#' @export

audit <- function(object, data=NULL, y = NULL, predict.function = yhat, residual.function = NULL, label=NULL){
  if(!is.null(data)) checkDataConsistency(data, y)
  UseMethod("audit")
}

#' @export
audit.default <- function(object, data=NULL, y = NULL, predict.function = yhat, residual.function = NULL, label=NULL){
  model <- object
  dataModel <- auditError(model, data, y)
  residual.function <- getResidualFunction(residual.function)

  residuals <- residual.function(model = model, data =  dataModel$data, y = dataModel$y, predict.function = predict.function)

  result <- list(
    model.class = class(model),
    label = ifelse(is.null(label), class(model)[1], label),
    model = model,
    fitted_values = predict.function(model, dataModel$data),
    data = dataModel$data,
    y = dataModel$y,
    predict.function = predict.function,
    residual.function = residual.function,
    residuals = residuals,
    std_residuals = residuals / sd(residuals)
  )
  class(result) <- "model_audit"
  return(result)
}

#' @export
audit.explainer <- function(object, data=NULL, y = NULL, predict.function = yhat, residual.function = NULL, label=NULL){
  explainer <- object
  if (is.null(data)) data <- explainer$data
  if (is.null(y)) y <- explainer$y
  if (is.null(label)) label <- explainer$label

  audit.default(
    object = explainer$model,
    data = data,
    y = y,
    predict.function = explainer$predict_function,
    residual.function = residual.function,
    label = label
    )
}






auditError <- function(model, data, y){
  if (is.null(data)) {
    possibleData <- try(model.frame(model), silent = TRUE)
    if (class(possibleData) != "try-error") {
      data <- possibleData
      if(is.null(y)) y <- data[, 1]
    } else { stop("data cannot be extracted from the model. The audit() function requires specified 'data' parameter.")
    }
  }
  if (is.null(y)) {stop("Original response cannot be extracted from the model. The audit() function requires specified 'y' parameter.") }
  return(list(data = data, y = y))
}





