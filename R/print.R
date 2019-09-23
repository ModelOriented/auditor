#' Prints Model Residual Summary
#'
#' @param x an object 'auditor_model_residual' created with \code{\link{model_residual}} function.
#' @param ... other parameters
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' titanic$survived <- titanic$survived == "yes"
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm, data = titanic, y = titanic$survived)
#'
#' # validate a model with auditor
#' library(auditor)
#' model_residual(exp_glm)
#'
#' @importFrom stats quantile
#'
#' @export
print.auditor_model_residual <- function(x, ...) {
  cat("Model label: ", levels(x[ ,"_label_"])[1], "\n")
  cat("Quantiles of Residuals:\n")
  print(quantile(x[,"_residuals_"], seq(0, 1, 0.1)))
  return(invisible(NULL))
}


#' Prints Model Cook's Distances Summary
#'
#' @param x an object 'auditor_model_cooksdistance' created with \code{\link{model_cooksdistance}} function.
#' @param ... other parameters
#'
#' @export
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # create an explainer
#' exp_lm <- DALEX::explain(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score
#' model_cooksdistance(exp_lm)
#'
#' @importFrom stats quantile
#'
#' @export
print.auditor_model_cooksdistance <- function(x, ...) {
  cat("Model label: ", levels(x[ ,"_label_"])[1], "\n")
  cat("\nObservations with the largest Cook's distances:\n")
  cd <- as.data.frame(x)
  colnames(cd)[c(1,3)] <- c("Cook's distance", "index")
  print(cd[1:3,c(1,3)])
  cat("\nQuantiles of Cook's distances:\n")
  print(quantile(x[,"_cooks_dist_"], seq(0, 1, 0.1), na.rm = TRUE))
  return(invisible(NULL))
}


#' Prints Model Evaluation Summary
#'
#' @param x an object 'auditor_model_evaluation' created with \code{\link{model_evaluation}} function.
#' @param ... other parameters
#'
#' @export
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- titanic$survived == "yes"
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm, data= titanic, y = titanic$survived,
#'                           predict_function = function(m, d)predict(m, newdata=d, type="response"))
#'
#' # validate a model with auditor
#' library(auditor)
#' model_evaluation(exp_glm)
#'
#'
#' @export
print.auditor_model_evaluation <- function(x, ...) {
  cat("Model label: ", levels(x[ ,"_label_"])[1], "\n")
  tpr <- round(x[which(x[,'_cutoffs_']<=0.5),'_tpr_'][1], 3)
  cat("\n True Positive Rate for cutoff 0.5:", tpr, "\n")
  fpr <- round(x[which(x[,'_cutoffs_']<=0.5),'_fpr_'][1], 3)
  cat("\n False Positive Rate for cutoff 0.5:", fpr, "\n")
  return(invisible(NULL))
}


#' Prints Model Halfnormal Summary
#'
#' @param x an object 'auditor_model_halfnormal' created with \code{\link{model_halfnormal}} function.
#' @param ... other parameters
#'
#' @export
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic[1:100,])
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_glm <- DALEX::explain(model_glm)
#'
#' # validate a model with auditor
#' library(auditor)
#' model_halfnormal(exp_glm)
#'
#'
#' @export
print.auditor_model_halfnormal <- function(x, ...) {
  cat("Model label: ", levels(x[ ,"_label_"])[1], "\n")
  cat("Quantiles of Residuals:\n")
  print(quantile(x[,"_residuals_"], seq(0, 1, 0.1)))
  return(invisible(NULL))
}


#' Prints Model Performance Summary
#'
#' @param x an object 'auditor_model_performance' created with \code{\link{model_performance}} function.
#' @param ... other parameters
#'
#' @export
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
#'
#' @export
print.auditor_model_performance <- function(x, ...) {
  cat("Model label: ", levels(x[ ,"_label_"])[1], "\n")
  x <- as.data.frame(x)
  colnames(x) <- c("score", "label", "name")
  print(x[,c(1,3)])
  return(invisible(NULL))
}




#' Prints of Models Scores
#'
#' @param x an object 'auditor_score' created with \code{\link{score}} function.
#' @param ... other parameters
#'
#' @export
#'
#' @examples
#' titanic <- na.omit(DALEX::titanic)
#' titanic$survived <- titanic$survived == "yes"
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic)
#' # create an explainer
#' exp_glm <- DALEX::explain(model_glm, y = titanic$survived)
#' # calculate score
#' score(exp_glm, score= "auc")
#'
#' @export
print.auditor_score <- function(x, ...) {
  cat(x$name, ":", x$score, "\n")
  return(invisible(NULL))
}

