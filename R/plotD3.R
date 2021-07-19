#' @title Model Diagnostic Plots in D3 with r2d3 package.
#'
#' @description This function provides several diagnostic plots for regression and classification models.
#' Provide object created with one of auditor's computational functions, \code{\link{model_residual}},
#' \code{\link{model_cooksdistance}}, \code{\link{model_evaluation}}, \code{\link{model_performance}},
#' \code{\link{model_evaluation}}.
#'
#' @param x object of class \code{auditor_model_residual} (created with \code{\link{model_residual}} function),
#' \code{auditor_model_performance} (created with \code{\link{model_performance}} function),
#' \code{auditor_model_evaluation} (created with \code{\link{model_evaluation}} function),
#' \code{auditor_model_cooksdistance} (created with \code{\link{model_cooksdistance}} function),
#' or \code{auditor_model_halfnormal} (created with \code{\link{model_halfnormal}} function).
#' @param ... other arguments dependent on the type of plot or additional objects of classes \code{'auditor_model_residual',
#' 'auditor_model_performance', 'auditor_model_evaluation', 'auditor_model_cooksdistance', 'auditor_model_halfnormal'}.
#' @param type the type of plot. Single character. Possible values:
#' \code{'acf', 'autocorrelation', 'cooksdistance', 'halfnormal','lift', 'prediction', 'rec', 'resiual',
#' 'roc', 'rroc', 'scalelocation'}, (for detailed description see corresponding functions in see also section).
#'
#' @seealso \code{\link{plotD3_acf}, \link{plotD3_autocorrelation}, \link{plotD3_cooksdistance},
#' \link{plotD3_halfnormal}, \link{plotD3_residual}, \link{plotD3_lift},
#' \link{plotD3_prediction}, \link{plotD3_rec}, \link{plotD3_roc},
#' \link{plotD3_rroc}, \link{plotD3_scalelocation}}
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # validate a model with auditor
#' mr_lm <- model_residual(lm_audit)
#'
#' # plot results
#' plotD3(mr_lm)
#' plotD3(mr_lm, type = "prediction")
#'
#' hn_lm <- model_halfnormal(lm_audit)
#' plotD3(hn_lm)
#'
#'
#' @importFrom grDevices devAskNewPage
#' @importFrom graphics plot
#'
#' @export
#' @rdname plotD3
plotD3 <- function(x, ...)
  UseMethod("plotD3")

#' @rdname plotD3
plotD3_auditor <- function(x, ..., type="residual"){

  object <- x

  plotNames <- c('acf', 'autocorrelation', 'cooksdistance', 'halfnormal', 'residual', 'lift',
                 'prediction', 'rec', 'roc', 'rroc', 'scalelocation')

  if(!all(type %in% plotNames)){
    stop(paste0("Invalid plot type. Possible values are: ", paste(plotNames, collapse = ", "),"."))
  }

  plotD3_selected_type(object, ..., type = type)
}


plotD3_selected_type <- function(x, ..., type){
  if("auditor_model_cooksdistance" %in% class(x)) type <- "cooksdistance"
  #if("auditor_model_performance" %in% class(x)) type <- "radar"
  if("auditor_model_halfnormal" %in% class(x)) type <- "halfnormal"
  if("auditor_model_evaluation" %in% class(x) & type != "lift") type <- "roc"


  switch(type,
         acf = { return(plotD3_acf(x, ...)) },
         autocorrelation = { return(plotD3_autocorrelation(x, ...)) },
         cooksdistance = { return(plotD3_cooksdistance(x, ...)) },
         halfnormal = { return(plotD3_halfnormal(x, ...)) },
         lift = {return(plotD3_lift(x, ...))},
         prediction = {return(plotD3_prediction(x, ...))},
         rec = { return(plotD3_rec(x, ...)) },
         residual = { return(plotD3_residual(x, ...)) },
         roc = { return(plotD3_roc(x, ...)) },
         rroc = { return(plotD3_rroc(x, ...)) },
         scalelocation = { return(plotD3_scalelocation(x, ...)) },
  )
}

#' @rdname plotD3
#' @export
plotD3.auditor_model_residual <- plotD3_auditor

#' @rdname plotD3
#' @export
plotD3.auditor_model_halfnormal <- plotD3_auditor

#' @rdname plotD3
#' @export
plotD3.auditor_model_evaluation <- plotD3_auditor

#' @rdname plotD3
#' @export
plotD3.auditor_model_cooksdistance <- plotD3_auditor

