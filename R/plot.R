#' @title Model Diagnostic Plots
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
#' @param type the type of plot. Character or vector of characters. Possible values: \code{'acf', 'autocorrelation', 'cooksdistance', 'halfnormal',
#' 'lift', 'pca', 'radar', 'correlation', 'prediction', 'rec', 'resiual', 'residual_boxplot','residual_density',
#' 'roc', 'rroc', 'scalelocation', 'tsecdf'} (for detailed description see corresponding functions in see also section).
#' @param ask logical; if \code{TRUE}, the user is asked before each plot, see \code{\link[graphics]{par}(ask=)}.
#' @param grid logical; if \code{TRUE} plots will be plotted on the grid.
#'
#' @seealso \code{\link{plot_acf}, \link{plot_autocorrelation}, \link{plot_cooksdistance},
#' \link{plot_halfnormal}, \link{plot_residual_boxplot}, \link{plot_lift}, \link{plot_pca},
#' \link{plot_radar}, \link{plot_correlation},
#' \link{plot_prediction}, \link{plot_rec}, \link{plot_residual_density}, \link{plot_residual}, \link{plot_roc},
#' \link{plot_rroc}, \link{plot_scalelocation}, \link{plot_tsecdf}}
#'
#' @return A ggplot object.
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
#' plot(mr_lm)
#' plot(mr_lm, type = "prediction")
#'
#' hn_lm <- model_halfnormal(lm_audit)
#' plot(hn_lm)
#'
#' library(randomForest)
#' model_rf <- randomForest(life_length~., data = dragons)
#' rf_audit <- audit(model_rf, data = dragons, y = dragons$life_length)
#'
#' mp_rf <- model_performance(rf_audit)
#' mp_lm <- model_performance(lm_audit)
#' plot(mp_lm, mp_rf)
#'
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import scales
#' @importFrom grDevices devAskNewPage
#'
#' @export
#'
#' @rdname plot
plot_auditor <- function(x, ..., type="residual", ask = TRUE, grid = TRUE){

  object <- x

  plotNames <- c('acf', 'autocorrelation', 'cooksdistance', 'halfnormal', 'residual', 'lift',
                 'pca', 'radar', 'correlation', 'prediction', 'rec', 'residual_boxplot', 'residual_density',
                 'roc', 'rroc', 'scalelocation', 'tsecdf')
  deprecated_names <- c('ModelRanking', 'REC', "ResidualBoxplot", 'ResidualDensity')
  if(!all(type %in% c(plotNames, deprecated_names))){
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
  ModelRanking <- NULL
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
         ModelRanking = {return(ModelRanking(x, ...))},

         correlation = {return(plot_correlation(x, ...))},
         prediction = {return(plot_prediction(x, ...))},
         rec = { return(plot_rec(x, ...)) },
         REC = { return(plotREC(x, ...)) },

         residual_boxplot = {return(plot_residual_boxplot(x, ...))},
         ResidualBoxplot = { return(plotResidualBoxplot(x, ...)) },

         residual_density = { return(plot_residual_density(x, ...)) },
         ResidualDensity = { return(plotResidualDensity(x, ...)) },

         residual = { return(plot_residual(x, ...)) },
         roc = { return(plot_roc(x, ...)) },
         rroc = { return(plot_rroc(x, ...)) },
         scalelocation = { return(plot_scalelocation(x, ...)) },
         tsecdf = { return(plot_tsecdf(x, ...)) }
  )
}

#' @rdname plot
#' @export
plot.auditor_model_residual <- plot_auditor

#' @rdname plot
#' @export
plot.auditor_model_performance <- plot_auditor

#' @rdname plot
#' @export
plot.auditor_model_halfnormal <- plot_auditor

#' @rdname plot
#' @export
plot.auditor_model_evaluation <- plot_auditor

#' @rdname plot
#' @export
plot.auditor_model_cooksdistance <- plot_auditor





