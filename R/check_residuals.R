#' Automated tests for model residuals
#'
#' Currently three tests are performed
#'  - for outliers in residuals
#'  - for autocorrelation in target variable or in residuals
#'  - for trend in residuals as a function of target variable (detection of bias)
#'
#' @param object An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param ... other parameters that will be passed to further functions.
#'
#' @return list with statistics for particualar checks
#' @export
#' @importFrom  stats cor.test loess
#' @importFrom  utils head tail
#'
#' @examples
#' library(DALEX)
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- explain(lm_model, data = dragons, y = dragons$life_length)
#' library(auditor)
#' check_residuals(lm_exp)
#'  \dontrun{
#'  library("ranger")
#'  rf_model <- ranger(life_length ~ ., data = dragons)
#'  predict_function <- function(m,x,...) predict(m, x, ...)$predictions
#'  rf_exp <- explain(rf_model, data = dragons, y = dragons$life_length,
#'            predict_function = predict_function)
#'  check_residuals(rf_exp)
#' }
check_residuals <- function(object, ...) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  autocorrelation <- check_residuals_autocorrelation(object, ...)
  outliers <- check_residuals_outliers(object, ...)
  trend <- check_residuals_trend(object, ...)

  return(invisible(list(autocorrelation = autocorrelation,
                        outliers = outliers,
                        trend = trend)))
}

#' Checks for outliers
#'
#' @param object  An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param n number of lowest and highest standardized  residuals to be presented
#'
#' @return indexes of lowest and highest standardized  residuals
#' @export
#'
#' @examples
#' library(DALEX)
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' library(auditor)
#' lm_exp <- explain(lm_model, data = dragons, y = dragons$life_length)
#' check_residuals_outliers(lm_exp)
check_residuals_outliers <- function(object, n = 5) {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")


  model_name <- object$label

  residuals <- object$residuals
  # standarisation
  stdresiduals <- (residuals - mean(residuals, na.rm = TRUE))/sd(residuals, na.rm = TRUE)
  sresiduals <- sort(abs(stdresiduals))
  norm <- qnorm(seq(0.5, 1 - 0.5/length(sresiduals),length.out = length(sresiduals)))

  shift <- c(mean(sresiduals - norm > 1),
             sum(sresiduals - norm > 1),
             mean(sresiduals - norm > 2),
             sum(sresiduals - norm > 2))

  ind_low <- head(order(residuals), n)
  ind_high  <- rev(tail(order(residuals), n))

  cat("  -----------------------------------------------\n")
  cat("   Checks for outliers\n")
  cat("  -----------------------------------------------\n")
  cat("    Model name: ", model_name, "\n")
  cat("    Shift > 1: ", round(shift[2]), "(",round(100*shift[1],1),"%)", "\n")
  cat("    Shift > 2: ", round(shift[4]), "(",round(100*shift[3],1),"%)", "\n")
  cat("    Top lowest standardised residuals: \n    ", paste0(signif(stdresiduals[ind_low], 5), " (",ind_low, ")", collapse = ", "), "\n")
  cat("    Top highest standardised residuals: \n    ", paste0(signif(stdresiduals[ind_high], 5), " (",ind_high, ")", collapse = ", "),"\n")
  invisible(list(ind_low = ind_low,
                 ind_high = ind_high,
                 shift = shift))
}

#' Checks for autocorrelation in target variable or in residuals
#'
#' @param object  An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param method will be passed to the cor.test functions
#'
#' @return autocorrelation between target variable and between residuals
#' @export
#'
#' @examples
#' library(DALEX)
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- explain(lm_model, data = dragons, y = dragons$life_length)
#' library(auditor)
#' check_residuals_autocorrelation(lm_exp)
check_residuals_autocorrelation <- function(object, method = "pearson") {
  if(!("explainer" %in% class(object))) stop("The function requires an object created with explain() function from the DALEX package.")

  model_name <- object$label

  y       <- object$y
  ctest_y <- cor.test(y[-1], y[-length(y)], method = method)
  y_autocorrelation <- ctest_y$estimate

  residuals <- object$residuals
  ctest_r   <- cor.test(residuals[-1], residuals[-length(residuals)], method = method)
  residual_autocorrelation <- ctest_r$estimate

  cat("  -----------------------------------------------\n")
  cat("   Checks for autocorrelation\n")
  cat("  -----------------------------------------------\n")
  cat("    Model name: ", model_name, "\n")
  cat("    Autocorrelation in target:    ", sprintf("%+1.2f", y_autocorrelation), "   ", stars(y_autocorrelation), "\n")
  cat("    Autocorrelation in residuals: ", sprintf("%+1.2f", residual_autocorrelation), "   ", stars(residual_autocorrelation),"\n")
  invisible(list(y_autocorelation = y_autocorrelation,
                 residual_autocorelation = residual_autocorrelation))
}

#' Checks for trend in residuals
#'
#' Calculates loess fit for residuals and then extracts statistics that shows how far is this fit from one without trend
#'
#' @param object  An object of class 'explainer' created with function \code{\link[DALEX]{explain}} from the DALEX package.
#' @param B number fo samplings
#'
#' @return standardized   loess fit for residuals
#' @export
#'
#' @examples
#' library(DALEX)
#' dragons <- DALEX::dragons[1:100, ]
#' lm_model <- lm(life_length ~ ., data = dragons)
#' lm_exp <- explain(lm_model, data = dragons, y = dragons$life_length)
#' library(auditor)
#' check_residuals_trend(lm_exp)
check_residuals_trend <- function(object, B = 20) {
  model_name <- object$label

  # calculates  smooth trend for fit
  df <- data.frame(residuals = object$residuals, y = object$y)
  fit <- loess(residuals ~ y, data = df)
  score0 <- sd(predict(fit))

  # sampling wise normalization per expected loess fit
  scores <- replicate(B, {
    df_random <- data.frame(residuals = sample(object$residuals), y = object$y)
    fit <- loess(residuals~y, data = df_random)
    sd(predict(fit))
  })
  # standardised loess fit
  coef0 <- abs(score0 - mean(scores))/sd(scores)

  cat("  -----------------------------------------------\n")
  cat("   Checks for trend in residuals\n")
  cat("  -----------------------------------------------\n")
  cat("    Model name: ", model_name, "\n")
  cat("    Standardised loess fit: ", sprintf("%+2.2f", coef0), "   ", stars(coef0, c(5,10,20,30)),"\n")
  invisible(list(loess_standardised_fit = coef0,
                 loess_sd_fit = score0))
}

stars <- function(x, breaks = c(0.1, 0.2, 0.3, 0.4)) {
  if (length(breaks) < 1 | x < breaks[1]) return("")
  if (length(breaks) < 2 | x < breaks[2]) return(".")
  return(paste(rep("*", sum(x > breaks) - 1), collapse = ""))
}


