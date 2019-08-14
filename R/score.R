#' @title Model Scores computations
#'
#' @description This function provides several scores for model validation and performance assessment.
#' Scores can be also used to compare models.
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param score The score to  be calculated. Possible values: 'auc' 'cookdistance', 'dw', 'peak', 'halfnormal', 'mae', 'mse', 'rec', 'rmse', 'rroc', 'runs'
#' (for detailed description see functions in see also section).
#' @param ... Other arguments dependent on the type of score.
#'
#' @seealso \code{\link{score_auc}}, \code{\link{score_cooksdistance}, \link{score_dw}, \link{score_peak}, \link{score_halfnormal}, \link{score_mae},
#' \link{score_mse}, \link{score_rec}, \link{score_rroc}, \link{score_runs}}
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
         runs = { return(score_runs(object, ...)) }
  )
  stop( "Wrong type of score. Possible values: 'auc', 'cooksdistance', 'dw', 'peak', 'halfnormal', 'rmse', 'mae', 'mse', 'rec', 'rroc', 'runs'." )
}
