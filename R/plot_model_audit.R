#' @title Model Diagnostic Plots
#'
#' @description This function provides several diagnostic plots for regression and classification models.
#' Provide object created with one of aduditor's computational functions, \code{\link{model_residual}},
#' \code{\link{model_cooksdistance}}, \code{\link{model_evaluation}}, \code{\link{model_performance}},
#' \code{\link{model_evaluation}}.
#'
#' @param x object of class 'auditor_model_residual' (created with \code{\link{model_residual}} function),
#' 'auditor_model_performance' (created with \code{\link{model_performance}} function),
#' 'auditor_model_evaluation' (created with \code{\link{model_evaluation}} function),
#' 'auditor_model_cooksdistance' (created with \code{\link{model_cooksdistance}} function),
#' or 'auditor_model_halfnormal' (created with \code{\link{model_halfnormal}} function).
#' @param ... other arguments dependent on the type of plot or additionl objects of classes 'auditor_model_residual',
#' 'auditor_model_performance', 'auditor_model_evaluation', 'auditor_model_cooksdistance', 'auditor_model_halfnormal'.
#' @param type the type of plot. Character or vector of characters. Possible values: 'acf', 'autocorrelation', 'cooksdistance', 'halfnormal', 'residuals',
#' 'lift', 'pca', 'radar', 'correlation', 'prediction', 'rec', 'resiual', 'residual_boxplot','residual_density',
#' 'roc', 'rroc', 'scalelocation', 'tsecdf' (for detailed description see corresponding functions in see also section).
#' @param ask logical; if TRUE, the user is asked before each plot, see \code{\link[graphics]{par}(ask=)}.
#' @param grid logical; if TRUE plots will be plotted on the grid.
#'
#' @seealso \code{\link{plot_acf}, \link{plot_autocorrelation}, \link{plot_cooksdistance},
#' \link{plot_halfnormal}, \link{plot_residual}, \link{plot_residual_boxplot}, \link{plot_lift}, \link{plot_pca},
#' \link{plot_radar}, \link{plot_correlation},
#' \link{plot_prediction}, \link{plot_rec}, \link{plot_residual_density}, \link{plot_residual}, \link{plot_roc},
#' \link{plot_rroc}, \link{plot_scalelocation}, \link{plot_tsecdf}}
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
#' # validate a model with auditor
#' library(auditor)
#' mr_lm <- model_residual(exp_lm)
#'
#' # plot results
#' plot(mr_lm)
#' plot(mr_lm, type = "prediction")
#'
#' hn_lm <- model_halfnormal(exp_lm)
#' plot(hn_lm)
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' exp_rf <- DALEX::explain(model_rf, data = dragons, y = dragons$life_length)
#'
#' mp_rf <- model_performance(exp_rf)
#' mp_lm <- model_performance(exp_lm)
#' plot(mp_lm, mp_rf)
#'
#'
#' @importFrom grDevices devAskNewPage
#' @importFrom graphics plot
#' @importFrom gridExtra grid.arrange
#'
#' @method plot model_audit
#'
#' @rdname plot.model_audit
#'
#' @export

plot.model_audit <- function(x, ..., type="residual", ask = TRUE, grid = TRUE){

  object <- x

  plotNames <- c('acf', 'autocorrelation', 'cooksdistance', 'halfnormal', 'residual', 'lift',
                 'pca', 'radar', 'correlation', 'prediction', 'rec', 'residual_boxplot', 'residual_density', 'residual',
                 'roc', 'rroc', 'scalelocation', 'tsecdf')
  if(!all(type %in% plotNames)){
    stop(paste0("Invalid plot type. Possible values are: ", paste(plotNames, collapse = ", "),"."))
  }

  if (length(type)==1) {
    return(plot_selected_type(object, ..., type = type))
  }

  if (ask & length(type) & (grid == FALSE)) {
    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
  }

  plotsList <- sapply(type, function(x) NULL)

  if(grid == TRUE) {
    for(name in type){
      plotsList[[name]] <- plot_selected_type(object, ..., type = name)
    }
    do.call(grid.arrange, args = plotsList)
  } else {
    for(name in type){
      plotsList[[name]] <- plot_selected_type(object, ..., type = name)
      plot(plotsList[[name]])
    }
    class(plotsList) <- c("auditor_plot_list", "list")
    return(plotsList)
  }

}



plot_selected_type <- function(x, ..., type){
  if("auditor_model_cooksdistance" %in% class(x)) type <- "cooksdistance"
  if("auditor_model_performance" %in% class(x)) type <- "radar"
  if("auditor_model_halfnormal" %in% class(x)) type <- "halfnormal"
  if("auditor_model_evaluation" %in% class(x) & type != "lift") type <- "roc"


  switch(type,
         acf = { return(plot_acf(x, ...)) },
         autocorrelation = { return(plot_autocorrelation(x, ...)) },
         cooksdistance = { return(plot_cooksdistance(x, ...)) },
         halfnormal = { return(plot_halfnormal(x, ...)) },
         lift = {return(plot_lift(x, ...))},
         pca = {return(plot_pca(x, ...))},
         radar = {return(plot_radar(x, ...))},
         correlation = {return(plot_correlation(x, ...))},
         prediction = {return(plot_prediction(x, ...))},
         rec = { return(plot_rec(x, ...)) },
         residual_boxplot = {return(plot_residual_boxplot(x, ...))},
         residual_density = { return(plot_residual_density(x, ...)) },
         residual = { return(plot_residual(x, ...)) },
         roc = { return(plot_roc(x, ...)) },
         rroc = { return(plot_rroc(x, ...)) },
         scalelocation = { return(plot_scalelocation(x, ...)) },
         tsecdf = { return(plot_tsecdf(x, ...)) }
  )
}


