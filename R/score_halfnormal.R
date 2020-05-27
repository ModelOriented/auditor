#' @title Half-Normal Score
#'
#' @description Score is approximately:
#' \eqn{ \sum{\#[res_i \leq simres_{i,j}] - n } }
#' with the distinction that each element of sum is also scaled to take values from [0,1].
#'
#' \eqn{res_i} is a residual for i-th observation, \eqn{simres_{i,j}} is the residual of j-th simulation
#' for i-th observation, and \eqn{n} is the number of simulations for each observation.
#' Scores are calculated on the basis of simulated data, so they may differ between function calls.
#'
#' @param object An object of class \code{explainer} created with function
#'  \code{\link[DALEX]{explain}} from the DALEX package.
#' @param ... ...
#'
#' @return An object of class \code{auditor_score}.
#'
#' @examples
#' dragons <- DALEX::dragons[1:100, ]
#'
#' # fit a model
#' model_lm <- lm(life_length ~ ., data = dragons)
#'
#' # create an explainer
#' lm_audit <- audit(model_lm, data = dragons, y = dragons$life_length)
#'
#' # calculate score
#' score_halfnormal(lm_audit)
#'
#'
#' @export

score_halfnormal <- function(object, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")
  object <- model_halfnormal(object)

  result <- list(
    name = "halfnormal",
    score = calculate_score_pdf(object)
  )

  class(result) <- "auditor_score"
  return(result)
}


# Calculating Likelihood for each residual
calculate_kde <- function(res, simres) {
  simres <- as.numeric(simres)
  (abs(sum(res<=simres) - length(simres)/2))/(length(simres)/2)
}


# Calculating PDF score
calculate_score_pdf <- function(hnpObject) {
  res <- hnpObject$`_residuals_`
  simres <- as.data.frame(t(hnpObject[,6:ncol(hnpObject)]))
  n <- length(res)
  PDFs <- mapply(calculate_kde, res, simres)
  return(sum(PDFs))
}

#' @rdname score_halfnormal
#' @export
scoreHalfNormal <- function(object, ...) {
  warning("Please note that 'scoreHalfNormal()' is now deprecated, it is better to use 'score_halfnormal()' instead.")
  score_halfnormal(object, ...)
}
