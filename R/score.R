#' @title Model Scores computations
#'
#' @description This function provides several scores for model validation and performance assessment.
#' Scores can be also used to compare models.
#'
#' @param object An object of class \code{explainer} created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param type The score to  be calculated. Possible values: \code{acc}, \code{auc}, \code{cookdistance}, \code{dw}, \code{f1},
#' \code{gini}, \code{halfnormal}, \code{mae}, \code{mse}, \code{peak},
#' \code{precision}, \code{r2}, \code{rec}, \code{recall}, \code{rmse},
#' \code{rroc}, \code{runs}, \code{specificity}, \code{one_minus_acc}, \code{one_minus_auc},
#' \code{one_minus_f1}, \code{one_minus_gini}, \code{one_minus_precision}, \code{one_minus_recall}, \code{one_minus_specificity}
#' (for detailed description see functions in see also section).
#' @param data New data that will be used to calculate the score. Pass \code{NULL} if you want to use \code{data} from \code{object}.
#' @param ... Other arguments dependent on the type of score.
#'
#' @seealso \code{\link{score_acc}}, \code{\link{score_auc}}, \code{\link{score_cooksdistance}}, \code{\link{score_dw}}, \code{\link{score_f1}},
#' \code{\link{score_gini}}  \code{\link{score_halfnormal}}, \code{\link{score_mae}}, \code{\link{score_mse}}, \code{\link{score_peak}},
#' \code{\link{score_precision}}, \code{\link{score_r2}}, \code{\link{score_rec}}, \code{\link{score_recall}}, \code{\link{score_rmse}},
#' \code{\link{score_rroc}}, \code{\link{score_runs}}, \code{\link{score_specificity}}, \code{\link{score_one_minus_acc}},
#' \code{\link{score_one_minus_auc}},
#' \code{\link{score_one_minus_f1}}, \code{\link{score_one_minus_gini}}, \code{\link{score_one_minus_precision}}, \code{\link{score_one_minus_recall}},
#' \code{\link{score_one_minus_specificity}}
#'
#' @return An object of class \code{auditor_score}, except Cooks distance, where numeric vector is returned.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score
#' score(lm_audit, type = 'mae')
#'
#' @export
#' @rdname score
score <- function(object, type = 'mse', data = NULL, ...) {

  #:# PLEASE keep this in alfabetical order - documentation too! PLEASE #:#
  switch(type,
         acc = { return(score_acc(object, data = data, ...)) },
         auc = { return(score_auc(object, data = data, ...)) },
         cooksdistance = { return(score_cooksdistance(object, ...)) },
         dw = { return(score_dw(object, data = data, ...)) },
         f1 = {return(score_f1(object, data = data, ...))},
         gini = {return(score_gini(object, data = data, ...))},
         halfnormal = { return(score_halfnormal(object, ...)) },
         mae = { return(score_mae(object, data = data, ...)) },
         mse = { return(score_mse(object, data = data, ...)) },
         peak = { return(score_peak(object, data = data, ...)) },
         precision = { return(score_precision(object, data = data, ...)) },
         r2 = { return(score_r2(object, data = data, ...)) },
         rec = { return(score_rec(object, data = data, ...)) },
         recall = { return(score_recall(object, data = data, ...)) },
         rmse = { return(score_rmse(object, data = data, ...)) },
         rroc = { return(score_rroc(object, data = data, ...)) },
         runs = { return(score_runs(object, data = data, ...)) },
         specificity = { return(score_specificity(object, data = data, ...)) },
         one_minus_acc = { return(score_one_minus_acc(object, data = data, ...)) },
         one_minus_auc = { return(score_one_minus_auc(object, data = data, ...)) },
         one_minus_f1 = {return(score_one_minus_f1(object, data = data, ...))},
         one_minus_gini = {return(score_one_minus_gini(object, data = data, ...))},
         one_minus_precision = { return(score_one_minus_precision(object, data = data, ...)) },
         one_minus_recall = { return(score_one_minus_recall(object, data = data, ...)) },
         one_minus_specificity = { return(score_one_minus_specificity(object, data = data, ...)) }
  )

  # 25

  stop( "Wrong type of score. Possible values: 'acc', 'auc', 'cooksdistance', 'dw', 'f1', 'gini', 'halfnormal',
        'mae', 'mse', 'peak', 'precision', 'r2', 'rec', 'recall', 'rmse', 'rroc', 'runs', 'specificity',
        'one_minus_acc', 'one_minus_auc', 'one_minus_f1', 'one_minus_gini','one_minus_precision', 'one_minus_recall', 'one_minus_specificity'." )
}
