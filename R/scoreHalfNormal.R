#' @title Half-Normal Score
#'
#' @description Score is approximately:
#' \eqn{ \sum{\#[res_i \leq simres_{i,j}] - n } }
#' with the distinction that each element of sum is also scaled to take values from [0,1].
#'
#' \eqn{res_i} is a residual for i-th observation, \eqn{simres_{i,j}} is the residual of j-th simulation for i-th observation, and \eqn{n} is the number of simulations for each observation.
#' Scores are calculated on the basis of simulated data, so they may differ between function calls.
#'
#' @param object modelAudit or modelFit object.
#' @param ... Extra arguments passed to \link[hnp]{hnp}.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_au <- audit(lm_model, data = dragons, y = dragons$life_length)
#' plotHalfNormal(lm_au)
#'
#'
#' @importFrom hnp hnp
#'
#' @export

scoreHalfNormal <- function(object, ...){
  if(!("modelFit" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelFit().")
  if("modelAudit" %in% class(object)) object <- modelFit(object)

  result <- list(
    name = "halfNormal",
    score = calculateScorePDF(object)
  )

  class(result) <- "scoreAudit"
  return(result)
}


# Calculating Likelihood for each residual
calculateKDE <- function(res, simres){
  simres <- as.numeric(simres)
  (abs(sum(res<=simres) - length(simres)/2))/(length(simres)/2)
}


# Calculating PDF score
calculateScorePDF <- function(hnpObject){
  res <- hnpObject$residuals
  simres <- as.data.frame(t(hnpObject[,6:ncol(hnpObject)]))
  n <- length(res)
  PDFs <- mapply(calculateKDE, res, simres)
  return(sum(PDFs))
}

