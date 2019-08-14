#' @title Create Model Performance Explaination
#'
#' @description  Creates 'modelPerformance'auditor_model_performance' object that can be used to plot radar with ranking of models.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param score Vector of score names to be plotted. Possible values are 'auc' 'cookdistance', 'dw', 'peak', 'halfnormal', 'mae', 'mse', 'rec', 'rmse', 'rroc', 'runs'
#' (for detailed description see functions in see also section).
#' @param new_score A named list of functions that take one argument: object of class 'explainer' and return a numeric value. The measure calculated by the function should have the property that lower score value indicates better model.
#'
#' @seealso \code{\link{score_auc}}, \code{\link{score_cooksdistance}, \link{score_dw}, \link{score_peak}, \link{score_halfnormal}, \link{score_mae},
#' \link{score_mse}, \link{score_rec}, \link{score_rroc}, \link{score_runs}}
#'
#' @return An object of the class 'auditor_model_performance'.
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- titanic$survived == "yes"
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm, data = titanic, y = titanic$survived)
#'
#' # validate a model with auditor
#' library(auditor)
#' model_performance(exp_glm)
#'
#' @export
model_performance <- function(object, score = c("mae", "mse", "rec", "rroc"), new_score = NULL) {
  check_object(object, type = "exp")
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
  colnames(df) <- c("_score_", "_label_", "_name_")
  class(df) <- c("auditor_model_performance", "data.frame")
  return(df)
}


#' @rdname model_performance
#'
#' @export
modelPerformance  <- function(object, score = c("mae", "mse", "rec", "rroc"), new_score = NULL) {
  message("Please note that 'modelPerformance()' is now deprecated, it is better to use 'model_performance()' instead.")
  model_performance(object, score, new_score)
}
