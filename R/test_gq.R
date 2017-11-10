#' @title Goldfeld-Quandt Test
#'
#' @description Test for homooskedasticity with the Goldfeld-Quandt Test.
#'
#' @param model model
#' @param variable name of variable - the observations in the model are ordered by the values of this variable. If NULL - first variable is taken.
#'
#' @importFrom dplyr arrange_
#' @importFrom stats update
#'
#' @export

test_gq <- function(model, variable = NULL){
  model.data <- model.frame(model)
  p <- ncol(model.data) - 1
  n <- nrow(model.data)
  n1 <- n%/%2
  n2 <- n - n1
  if (is.null(variable)) {
    variable <- colnames(model.data)[2]
  }

  model.data <- arrange_(model.data, variable)

  model.dataA <- model.data[1:n1, ]
  model.dataB <- model.data[(n1+1):n, ]

  modelA <- update(model, data = model.dataA)
  modelB <- update(model, data = model.dataB)

  augA <- augment(modelA)
  RSSA <- sum(augA$.resid^2)
  augB <- augment(modelB)
  RSSB <- sum(augB$.resid^2)

  GQ <- (RSSB/(n2 - p - 1)) / (RSSA/(n1 - p - 1))
  df <- c(n2 - p - 1, n1 - p - 1)
  names(df) <- c("df1", "df2")

  res <- list(
    statistic = GQ,
    parameter = df,
    p.value = (2 * min(pf(GQ, df[1], df[2]), pf(GQ, df[1], df[2], lower.tail = FALSE))),
    name = "Goldfeld-Quandt",
    values = list(model.dataA[ ,variable], model.dataB[ ,variable]),
    residuals = list(augA$.resid, augB$.resid),
    variable = variable
  )

return(res)
}

