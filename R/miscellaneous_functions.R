order_residuals <- function(object, variable){
  ordered_df <- data.frame(residuals = {object$residuals},
                      std_residuals = {object$residuals / sd(object$residuals)},
                      y = {object$y},
                      y_hat = {object$y_hat}
                      )

  if (is.null(variable)) {
    ordered_df$values <- object$y
  } else if (variable == "") {
    ordered_df$values <- 1:nrow(object$data)
  } else {
    ordered_df$values <- object$data[ ,variable]
  }

  ordered_df$index <- rownames(object$data)
  ordered_df <- ordered_df[order(ordered_df$values), ]

  ordered_df
}
