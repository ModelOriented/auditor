#' @title Model diagnostioc plots
#'
#' @description This function provides many scores
#'
#' @param x object of class ModelAudit
#' @param type the type of score to  be calculated. Possible values: 'ACF', 'Autocorrelation', 'Cook',
#' 'HalfNormal', 'Residuals', 'ScaleLocation' (see the details section).
#' @param ... other arguments dependent on the type of plot.
#'
#' @details TODO: desription of plot types
#'
#' @seealso \code{\link{plotACF}, \link{plotAutocorrelation}, \link{plotCook}, \link{plotHalfNormal},
#' \link{plotResiduals}, \link{plotScaleLocation}}
#'
#' @export

plot.modelAudit <- function(x, type="Residuals", ...){

  switch(type,
         ACF={ return(plotACF(x, ...)) },
         Autocorrelation={ return(plotAutocorrelation(x, ...)) },
         Cook={ return(plotCook(x, ...)) },
         HalfNormal={ return(plotHalfNormal(x, ...)) },
         Residuals = { return(plotResiduals(x, ...)) },
         ScaleLocation = { return(plotScaleLocation(x, ...)) })

  stop( "Wrong type of plot. Possible values: 'ACF', 'Autocorrelation', 'Cook', 'HalfNormal', 'Residuals', 'ScaleLocation'." )
}
