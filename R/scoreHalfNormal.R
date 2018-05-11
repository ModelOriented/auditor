#' @title Half-Normal Score
#'
#' @description Score is approximately:
#' \eqn{ \sum{\#[res_i \leq simres_{i,j}] - n } }
#' with the distinction that each element of sum is also scaled to take values from [0,1].
#'
#' \eqn{res_i} is a residual for i-th observation, \eqn{simres_{i,j}} is the residual of j-th simulation for i-th observation, and \eqn{n} is the number of simulations for each observation.
#' Scores are calculated on the basis of simulated data, so they may differ between function calls.
#'
#' @param object ModelAudit object or fitted model.
#' @param ... Extra arguments passed to \link[hnp]{hnp}.
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige~education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' plotHalfNormal(lm_au)
#'
#'
#' @importFrom hnp hnp
#'
#' @export

scoreHalfNormal <- function(object, ...){
  if(class(object)=="modelAudit") object <- object$model

  hnpObject <- hnp(object,...)
  result <- list(
    name = "halfNormal",
    score = calculateScorePDF(hnpObject)
  )

  class(result) <- "scoreAudit"
  return(result)
}



# calculateScorePDF fucntion is in plotHalfNormal.R file
