#' @title Prints Model Residual Summary
#'
#' @param x an object \code{auditor_model_residual} created with \code{\link{model_residual}} function.
#' @param ... other parameters
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' model_residual(glm_audit)
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


#' @title Prints Model Cook's Distances Summary
#'
#' @param x an object \code{auditor_model_cooksdistance} created with \code{\link{model_cooksdistance}} function.
#' @param ... other parameters
#'
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # create an explainer
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score
#' model_cooksdistance(lm_audit)
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


#' @title Prints Model Evaluation Summary
#'
#' @param x an object \code{auditor_model_evaluation} created with \code{\link{model_evaluation}} function.
#' @param ... other parameters
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data= titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' model_evaluation(glm_audit)
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


#' @title Prints Model Halfnormal Summary
#'
#' @param x an object \code{auditor_model_halfnormal} created with \code{\link{model_halfnormal}} function.
#' @param ... other parameters
#'
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' model_halfnormal(glm_audit)
#'
#'
#' @export
print.auditor_model_halfnormal <- function(x, ...) {
  cat("Model label: ", levels(x[ ,"_label_"])[1], "\n")
  cat("Quantiles of Residuals:\n")
  print(quantile(x[,"_residuals_"], seq(0, 1, 0.1)))
  return(invisible(NULL))
}


#' @title Prints Model Performance Summary
#'
#' @param x an object \code{auditor_model_performance} created with \code{\link{model_performance}} function.
#' @param ... other parameters
#'
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' model_performance(glm_audit)
#'
#' @export
print.auditor_model_performance <- function(x, ...) {
  cat("Model label: ", levels(x[ ,"_label_"])[1], "\n")
  x <- as.data.frame(x)
  colnames(x) <- c("score", "label", "name")
  print(x[,c(1,3)])
  return(invisible(NULL))
}




#' @title Prints of Models Scores
#'
#' @param x an object \code{auditor_score} created with \code{\link{score}} function.
#' @param ... other parameters
#'
#' @export
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#' # calculate score
#' score(glm_audit, type = "auc")
#'
#' @export
print.auditor_score <- function(x, ...) {
  cat(x$name, ": ", x$score, sep="")
  return(invisible(NULL))
}

