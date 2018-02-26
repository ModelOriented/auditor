#' @title Create modelAudit object
#'
#' @description Function \code{audit} create ModelAudit object for further validation of model.
#' Models may have very different structures. This function creates a unified representation of a model and calculate residuals,
#' which can be further processed by various error analysis functions.
#'
#' @param model An object containing a model.
#' @param data data.frame or matrix - data used for fitting. If not provided, will be extracted from the model.
#' @param y Response used for building a model.
#' @param predict.function Function that takes two arguments: model and new data and returns numeric vector with predictions.
#'
#' @return An object of class ModelAudit, which contains:
#' #' \itemize{
#' \item \code{model.class} class of the audited model,
#' \item \code{model} the audited model,
#' \item \code{fitted.values} fitted values from model,
#' \item \code{data} data used for fitting the model,
#' \item \code{y} vecor with values of predicted variable used for fittng the model,
#' \item \code{predict.function} function that may be used for model predictions, shall return a single numerical value for each observation.
#' \item \code{residuals}
#' \item \code{std.residuals} standardized residuals - the residuals divided by theirs standard deviation.
#' }
#'
#'
#' @importFrom stats model.frame
#'
#' @examples
#' library(MASS)
#' model.glm <- glm(Postwt ~ Prewt + Treat + offset(Prewt), family = gaussian, data = anorexia)
#' audit.glm <- audit(model.glm)
#'
#' p.fun <- function(model, data){predict(model, data, response = "link")}
#' audit.glm.newpred <- audit(model.glm, predict.function = p.fun)
#'
#'
#' library(randomForest)
#' model.rf <- randomForest(Species ~ ., data=iris)
#' audit.rf <- audit(model.rf)
#'
#' @export



audit <- function(model, data=NULL, y = NULL, predict.function = NULL){

  dataModel <- auditError(model, data, y)
  predict.function <- getPredictFunction(model, predict.function)

  result <- list(
    model.class = class(model),
    model = model,
    fitted.values = predict.function(model, dataModel$data),
    data = dataModel$data,
    y = dataModel$y,
    predict.function = predict.function,
    residuals = getResiduals(model, dataModel$y, dataModel$data, predict.function),
    std.residuals = getStdResiduals(model, dataModel$y, dataModel$data, predict.function)
  )
  class(result) <- "modelAudit"
  return(result)
}

auditError <- function(model, data, y){
  if (is.null(data)) {
    possibleData <- try(model.frame(model), silent = TRUE)
    if (class(possibleData) != "try-error") {
      data <- possibleData
      y <- data[, 1]
    } else { stop("data cannot be extracted from the model. The audit() function requires specified 'data' parameter.")
    }
  }
  if (is.null(y)) {stop("Original response cannot be extracted from the model. The audit() function requires specified 'y' parameter.") }
  return(list(data = data, y = y))
}
