#' @title Create Model Performance Explainer
#'
#' @description  Creates modelPerformance object to be plotted.
#'
#' @param object An object of class ModelAudit.
#' @param score Vector of score names to be plotted.
#' @param new_score A named list of functions that take one argument: object of class ModelAudit and return a numeric value. The measure calculated by the function should have the property that lower score value indicates better model.
#' @param ... other parameters.
#'
#' @return An object of the class 'auditor_model_performance'.
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm, y = titanic$survived)
#'
#' model_performance(audit_glm)
#'
#'
#' @export
model_performance <- function(object, score = c("mae", "mse", "rec", "rroc"), new_score = NULL) {
  if (!("model_audit" %in% class(object))) stop("The function requires an object created with audit().")

    score <- sapply(score, function(x) score(object, score = x)$score)
    df <- data.frame(score = score[1], label = object$label, name = names(score[1]))


    if (length(score) > 1) df <- rbind(df, data.frame(score = score[-1], label = object$label, name = names(score[-1])))

    if (!is.null(new_score)) {
      if (class(new_score) == "function") {
        df <- rbind(df, data.frame(score = new_score(object), label = object$label, name = as.character(substitute(new_score))))
      }
      if(class(new_score) == "list") {
        for (i in names(new_score)) {
          df <- rbind(df, data.frame(score = new_score[[i]](object), label = object$label, name = i))
        }
      }
    }

  class(df) <- c("auditor_model_performance", "data.frame")
  return(df)
}


#' @title Create Model Performance Explainer
#'
#' @description  Creates modelPerformance object to be plotted.
#'
#' @param object An object of class ModelAudit.
#' @param scores Vector of score names to be plotted.
#' @param new.score A named list of functions that take one argument: object of class ModelAudit and return a numeric value. The measure calculated by the function should have the property that lower score value indicates better model.
#' @param ... other parameters.
#'
#' @return An object of the class 'model_performance'.
#'
#' @examples
#' library(DALEX)
#' data(titanic)
#' titanic <- na.omit(titanic)
#' titanic$survived <- titanic$survived == "yes"
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' audit_glm <- audit(model_glm, y = titanic$survived)
#'
#' modelPerformance(audit_glm)
#'
#' @export
modelPerformance  <- function(object, scores = c("mae", "mse", "rec", "rroc"), new.score = NULL) {
  message("Please note that 'modelPerformance()' is now deprecated, it is better to use 'model_performance()' instead.")
  model_performance(object, scores, new.score)
}
