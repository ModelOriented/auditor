#' @title Model Scores computations
#'
#' @description This function provides several scores for model validation and performance assessment.
#' Scores can be also used to compare models.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param score The score to  be calculated. Possible values: 'auc' 'cookdistance', 'dw', 'peak', 'halfnormal', 'mae', 'mse', 'rec', 'rmse', 'rroc', 'runs'
#' 'one_minus_auc', 'one_minus_acc', 'one_minus_f1', 'one_minus_precision', 'one_minus_recall', 'one_minus_specificity', 'acc', 'f1', 'precision', 'recall', 'specificity'
#' (for detailed description see functions in see also section).
#' @param ... Other arguments dependent on the type of score.
#'
#' @seealso \code{\link{score_auc}}, \code{\link{score_cooksdistance}}, \code{\link{score_dw}}, \code{\link{score_peak}}, \code{\link{score_halfnormal}}, \code{\link{score_mae}},
#' \code{\link{score_mse}}, \code{\link{score_rec}}, \code{\link{score_rroc}}, \code{\link{score_runs}}, \code{\link{score_one_minus_auc}}, \code{\link{score_one_minus_acc}},
#' \code{\link{score_one_minus_f1}}, \code{\link{score_one_minus_precision}},
#' \code{\link{score_one_minus_recall}}, \code{\link{score_one_minus_specificity}}, \code{\link{score_acc}}, \code{\link{score_f1}}, \code{\link{score_precision}},
#' \code{\link{score_recall}}, \code{\link{score_specificity}}
#'
#' @return An object of class 'auditor_score', except Cooks distance, where numeric vector is returned.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # use DALEX package to wrap up a model into explainer
#' exp_lm <- DALEX::explain(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score
#' score(exp_lm, score = 'mae')
#'
#' @export

score <- function(object, score = 'mse', ...){

  switch(score,
         cooksdistance = { return(score_cooksdistance(object, ...)) },
         dw = { return(score_dw(object, ...)) },
         peak = { return(score_peak(object, ...)) },
         halfnormal = { return(score_halfnormal(object, ...)) },
         mse = { return(score_mse(object, ...)) },
         mae = { return(score_mae(object, ...)) },
         rec = { return(score_rec(object, ...)) },
         rmse = { return(score_rmse(object, ...)) },
         auc = { return(score_auc(object, ...)) },
         rroc = { return(score_rroc(object, ...)) },
         runs = { return(score_runs(object, ...)) },
         one_minus_auc = { return(score_one_minus_auc(object, ...)) },
         one_minus_acc = { return(score_one_minus_acc(object, ...)) },
         one_minus_f1 = {return(score_one_minus_f1(object, ...))},
         one_minus_precision = { return(score_one_minus_precision(object, ...)) },
         one_minus_recall = { return(score_one_minus_recall(object, ...)) },
         one_minus_specificity = { return(score_one_minus_specificity(object, ...)) },
         acc = { return(score_acc(object, ...)) },
         f1 = {return(score_f1(object, ...))},
         precision = { return(score_precision(object, ...)) },
         recall = { return(score_recall(object, ...)) },
         specificity = { return(score_specificity(object, ...)) }

  )
  stop( "Wrong type of score. Possible values: 'auc', 'cooksdistance', 'dw', 'peak', 'halfnormal', 'rmse', 'mae', 'mse', 'rec', 'rroc', 'runs', 'one_minus_auc', 'one_minus_acc', 'one_minus_f1', 'one_minus_precision', 'one_minus_recall', 'one_minus_specificity', 'acc', 'f1', 'precision', 'recall', 'specificity'." )
}
