#' @title Model diagnostic plots
#'
#' @description This function provides several diagnostic plots for regression and classification models.
#'
#' @param x object of class modelAudit
#' @param ... other arguments dependent on the type of plot or additionam objects of class modelAudit
#' @param type the type of plot. Possible values: 'ACF', 'Autocorrelation', 'Cook',
#' 'HalfNormal', 'Residuals', 'ScaleLocation', 'ROC', 'RROC', 'REC' (for detailed description see functions in seealso section).
#'
#' @seealso \code{\link{plotACF}, \link{plotAutocorrelation}, \link{plotCook}, \link{plotHalfNormal},
#' \link{plotResiduals}, \link{plotScaleLocation}, \link{plotROC}, \link{plotRROC}, \link{plotREC}}
#'
#' @export

plot.modelAudit <- function(x, ..., type="Residuals"){

  switch(type,
         ACF={ return(plotACF(x, ...)) },
         Autocorrelation={ return(plotAutocorrelation(x, ...)) },
         Cook={ return(plotCook(x, ...)) },
         HalfNormal={ return(plotHalfNormal(x, ...)) },
         Residuals = { return(plotResiduals(x, ...)) },
         ScaleLocation = { return(plotScaleLocation(x, ...)) },
         ROC = { return(plotROC(x, ...)) },
         REC = { return(plotREC(x, ...)) },
         RROC = { return(plotRROC(x, ...)) })

  stop( "Wrong type of plot. Possible values: 'ACF', 'Autocorrelation', 'Cook', 'HalfNormal', 'Residuals', 'ScaleLocation', 'ROC', 'RROC', 'REC'." )
}
