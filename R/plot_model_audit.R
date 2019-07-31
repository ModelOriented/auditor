#' @title Model diagnostic plots
#'
#' @description This function provides several diagnostic plots for regression and classification models.
#'
#' @param x object of class 'model_audit', 'model_residual', 'auditor_model_performance', 'model_evaluation',
#' 'model_cooksdistance', or 'model_halfnormal'.
#' @param ... other arguments dependent on the type of plot or additionl objects of class modelAudit
#' @param type the type of plot. Possible values: 'acf', 'autocorrelation', 'cooksdistance', 'halfnormal', 'residuals',
#' 'lift', 'pca', 'radar', 'correlation', 'prediction', 'rec', 'resiual', 'residual_boxplot','residual_density',
#' 'roc', 'rroc', 'scalelocation', 'tsecdf' (for detailed description see functions in see also section).
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
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plot(lm_au)
#'
#' library(randomForest)
#' rf_model <- randomForest(life_length~., data = dragons)
#' rf_au <- audit(rf_model, data = dragons, y = dragons$life_length)
#' plot(lm_au, rf_au, type = "radar")
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


