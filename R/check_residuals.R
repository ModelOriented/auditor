#' Automated tests for model residuals
#'
#' Currently three tests are performed
#'  - for outliers in residuals
#'  - for autocorrelation in target variable or in residuals
#'  - for trend in residuals as a function of target variable (detection of bias)
#'
#' @param model An object of class `modelAudit`
#' @param ... other parameters that will be passed to further functions
#'
#' @return list with statistics for particualr checks
#' @export
#' @importFrom  stats cor.test loess
#' @importFrom  utils head tail
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige ~ education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' check_residuals(lm_au)
#'  \dontrun{
#'  library("DALEX2")
#'  library("ranger")
#'  predict_function <- function(m,x,...) predict(m, x, ...)$predictions
#'  model_old <- ranger(m2.price ~ ., data = apartments)
#'  rf_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#'  check_residuals(rf_au)
#' }
check_residuals <- function(model, ...) {
  autocorrelation <- check_residuals_autocorrelation(model, ...)
  outliers <- check_residuals_outliers(model, ...)
  trend <- check_residuals_trend(model, ...)

  return(invisible(list(autocorrelation = autocorrelation,
                        outliers = outliers,
                        trend = trend)))
}

#' Checks for outliers
#'
#' @param model an object of the class `modelAudit`
#' @param n number of lowest and highest standardized  residuals to be presented
#'
#' @return indexes of lowest and highest standardized  residuals
#' @export
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige ~ education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' check_residuals_outliers(lm_au)
check_residuals_outliers <- function(model, n = 5) {
  model_name <- model$label

  residuals <- model$residuals
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
#' @param model an object of the class `modelAudit`
#' @param method will be passed to the cor.test functions
#'
#' @return autocorrelation between target variable and between residuals
#' @export
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige ~ education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' check_residuals_autocorrelation(lm_au)
check_residuals_autocorrelation <- function(model, method = "pearson") {
  model_name <- model$label

  y       <- model$y
  ctest_y <- cor.test(y[-1], y[-length(y)], method = method)
  y_autocorrelation <- ctest_y$estimate

  residuals <- model$residuals
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
#' @param model an object of the class `modelAudit`
#' @param B number fo samplings
#'
#' @return standardized   loess fit for residuals
#' @export
#'
#' @examples
#' library(car)
#' lm_model <- lm(prestige ~ education + women + income, data = Prestige)
#' lm_au <- audit(lm_model, data = Prestige, y = Prestige$prestige)
#' check_residuals_trend(lm_au)
check_residuals_trend <- function(model, B = 20) {
  model_name <- model$label

  # calculates  smooth trend for fit
  df <- data.frame(residuals = model$residuals, y = model$y)
  fit <- loess(residuals ~ y, data = df)
  score0 <- sd(predict(fit))

  # sampling wise normalization per expected loess fit
  scores <- replicate(B, {
    df_random <- data.frame(residuals = sample(model$residuals), y = model$y)
    fit <- loess(residuals~y, data = df_random)
    sd(predict(fit))
  })
  # standardised loess fit
  coef0 <- abs(score0 - mean(scores))/sd(scores)

  cat("  -----------------------------------------------\n")
  cat("   Checks for trend in residuals\n")
  cat("  -----------------------------------------------\n")
  cat("    Model name: ", model_name, "\n")
  cat("    Standardised loess fit: ", sprintf("%+2.2f", coef0), "   ", stars(coef0, c(3,5,10,20)),"\n")
  invisible(list(loess_standardised_fit = coef0,
                 loess_sd_fit = score0))
}

stars <- function(x, breaks = c(0.1, 0.2, 0.3, 0.4)) {
  if (length(breaks) < 1 | x < breaks[1]) return("")
  if (length(breaks) < 2 | x < breaks[2]) return(".")
  return(paste(rep("*", sum(x > breaks) - 1), collapse = ""))
}


