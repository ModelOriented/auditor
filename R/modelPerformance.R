#' @title Create Model Performance Explainer
#'
#' @description  Creates observationInfluence object to be plotted.
#'
#' @param object An object of class ModelAudit.
#' @param type Vector of score names to be plotted.
#' @param new.score A named list of functions that take one argument: object of class ModelAudit and return a numeric value. The measure calculated by the function should have the property that lower score value indicates better model.
#' @param ... other parameters.
#'
#' @export
modelPerformance <- function(object, type = c("MAE", "MSE", "REC", "RROC"), new.score = NULL){
  if(!("modelAudit" %in% class(object))) stop("The function requires an object created with audit().")


    df <- data.frame(score = score(object, type = type[1])$score, label = object$label, name = type[1])

    if(length(type) > 1){
      for(i in 2:length(type)){
        df <- rbind(df, data.frame(score = score(object, type = type[i])$score, label = object$label, name = type[i]))
      }
    }

    if(!is.null(new.score)){
      if (class(new.score) == "function"){
        df <- rbind(df, data.frame(score = new.score(object), label = object$label, name = as.character(substitute(new.score))))
      }
      if(class(new.score) == "list") {
        for(i in names(new.score)){
          df <- rbind(df, data.frame(score = new.score[[i]](object), label = object$label, name = i))
        }
      }
    }
  result <- df
  class(result) <- c("modelPerformance", "data.frame")

  return(result)
}
