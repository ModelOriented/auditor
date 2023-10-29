#' @title Create Model Performance Explanation
#'
#' @description  Creates \code{auditor_model_performance} object that can be used to plot radar with ranking of models.
#'
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param score Vector of score names to be calculated. Possible values: \code{acc}, \code{auc}, \code{cookdistance}, \code{dw}, \code{f1},
#' \code{gini}, \code{halfnormal}, \code{mae}, \code{mse}, \code{peak},
#' \code{precision}, \code{r2}, \code{rec}, \code{recall}, \code{rmse},
#' \code{rroc}, \code{runs}, \code{specificity},  \code{one_minus_acc}, \code{one_minus_auc},
#' \code{one_minus_f1}, \code{one_minus_gini}, \code{one_minus_precision}, \code{one_minus_recall}, \code{one_minus_specificity}
#' (for detailed description see functions in see also section). Pass \code{NULL} if you want to use only custom scores by \code{new_score} parameter.
#' @param new_score A named list of functions that take one argument: object of class 'explainer' and return a numeric value. The measure calculated by the function should have the property that lower score value indicates better model.
#' @param data New data that will be used to calculate scores. Pass \code{NULL} if you want to use \code{data} from \code{object}.
#' @param ... Other arguments dependent on the score list.
#'
#' @seealso \code{\link{score_acc}}, \code{\link{score_auc}}, \code{\link{score_cooksdistance}}, \code{\link{score_dw}},
#' \code{\link{score_f1}}, \code{\link{score_gini}},
#' \code{\link{score_halfnormal}}, \code{\link{score_mae}}, \code{\link{score_mse}},
#' \code{\link{score_peak}}, \code{\link{score_precision}}, \code{\link{score_r2}},
#' \code{\link{score_rec}}, \code{\link{score_recall}}, \code{\link{score_rmse}},
#' \code{\link{score_rroc}}, \code{\link{score_runs}}, \code{\link{score_specificity}},
#' \code{\link{score_one_minus_acc}}, \code{\link{score_one_minus_auc}}, \code{\link{score_one_minus_f1}},
#' \code{\link{score_one_minus_precision}}, \code{\link{score_one_minus_gini}},
#' \code{\link{score_one_minus_recall}}, \code{\link{score_one_minus_specificity}}
#'
#' @return An object of the class \code{auditor_model_performance}.
#'
#' @examples
#' data(titanic_imputed, package = "DALEX")
#'
#' # fit a model
#' model_glm <- glm(survived ~ ., family = binomial, data = titanic_imputed)
#'
#' # use DALEX package to wrap up a model into explainer
#' glm_audit <- audit(model_glm,
#'                    data = titanic_imputed,
#'                    y = titanic_imputed$survived)
#'
#' # validate a model with auditor
#' library(auditor)
#' mp <- model_performance(glm_audit)
#' mp
#'
#' plot(mp)
#'
#' @export
model_performance <- function(object, score = c("mae", "mse", "rec", "rroc"), new_score = NULL, data = NULL, ...) {

  check_object(object, type = "exp")

  # inject new data to the explainer
  if (!is.null(data)) object$data <- data

  if (!is.null(score)) {
    score <- sapply(score, function(x) score(object, type = x, ...)$score)
    df <- data.frame(score = score[1], label = object$label, name = names(score[1]),
                     stringsAsFactors = TRUE)
    if (length(score) > 1) df <- rbind(df, data.frame(score = score[-1], label = object$label, name = names(score[-1]),
                                                      stringsAsFactors = TRUE))
  } else {
    df <- data.frame(score = numeric(), label = factor(), name = character(),
                     stringsAsFactors = TRUE)
  }


  if (!is.null(new_score)) {
    if (inherits(new_score, "function")) {
      df <- rbind(df, data.frame(score = new_score(object), label = object$label, name = as.character(substitute(new_score)),
                                 stringsAsFactors = TRUE))
    }
    if (inherits(new_score, "list")) {
      for (i in names(new_score)) {
        df <- rbind(df, data.frame(score = new_score[[i]](object), label = object$label, name = i,
                                   stringsAsFactors = TRUE))
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
  warning("Please note that 'modelPerformance()' is now deprecated, it is better to use 'model_performance()' instead.")
  model_performance(object, score, new_score)
}
