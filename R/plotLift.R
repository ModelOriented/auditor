#' @title LIFT
#'
#' @description LIFT is a plot of the rate of positive prediction against true positive rate for the different thresholds.
#' It is useful for measuring and comparing the accuracy of the classificators.
#'
#' @param object An object of class modelAudit or modelEvaluation.
#' @param ... Other modelAudit objects to be plotted together.
#'
#' @return ggplot object
#'
#' @seealso \code{\link{plot.modelAudit}}
#'
#' @examples
#' library(mlbench)
#' data("PimaIndiansDiabetes")
#' Pima <- PimaIndiansDiabetes
#' Pima$diabetes <- ifelse(Pima$diabetes == "pos", 1, 0)
#' glm_model <- glm(diabetes~., family=binomial,	data=Pima)
#' glm_au <- audit(glm_model, data = Pima, y = Pima$diabetes)
#' plotLIFT(glm_au)
#'
#' @import ggplot2
#' @importFrom ROCR performance prediction
#'
#'
#' @export


plotLIFT <- function(object, ...){
  if(!("modelEvaluation" %in% class(object) || "modelAudit" %in% class(object))) stop("The function requires an object created with audit() or modelEvaluation().")
  if("modelAudit" %in% class(object)) object <- modelEvaluation(object)
  rpp <- tp <- label <- NULL

  df <- attributes(object)$CGains
  idealdf <- attributes(object)$idealCGains
  idealdf <- rbind(idealdf, c(0, 0, 0, "ideal"))
  idealdf$tp <- as.numeric(idealdf$tp)
  idealdf$rpp <- as.numeric(idealdf$rpp)
  idealdf$alpha <- as.numeric(idealdf$alpha)

  randomdf <- data.frame(rpp = c(0, 1), tp = c(0, max(idealdf$tp)), alpha = c(0, 1),
                                               label =c("random", "random"))

  dfl <- list(...)
  if (length(dfl) > 0) {
    for (resp in dfl) {
      if("modelAudit" %in% class(resp)) resp <- modelEvaluation(resp)
      if("modelEvaluation" %in% class(resp))  df <- rbind( df, attributes(resp)$CGains )
    }
  }

  for(lab in unique(df$label)) df <- rbind(df, c("0", "0", "0", lab))
  df$tp <- as.numeric(df$tp)
  df$rpp <- as.numeric(df$rpp)
  df$alpha <- as.numeric(df$alpha)

  ggplot(df, aes(x = rpp, y = tp, color = label)) +
    geom_line() +
    geom_line(data = idealdf, aes(x = rpp, y = tp), color = "orange") +
    geom_line(data = randomdf, aes(x = rpp, y = tp), color = "black") +
    xlab("rate of positive prediction") +
    ylab("true positive") +
    ggtitle("LIFT Chart") +
    theme_light()
}




