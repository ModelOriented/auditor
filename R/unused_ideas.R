# ######### plots for Goldfeld-Quandt test #########
# plot_gqtest2 <- function(ModelAudit){
#   variable <- ModelAudit$gqtest$test$variable
#   dt <- data.frame(y = ModelAudit$data[ ,1], variable = ModelAudit$data[,variable])
#   dt <- dt[order(dt$variable), ]
#   dt$residuals <- c(ModelAudit$gqtest$test$residuals1, ModelAudit$gqtest$test$residuals2)
#
#   n <- nrow(dt)
#   n1 <- n%/%2
#   dt$median <- c(rep("< cut-point1", n1), rep("> cut-point2", n-n1) )
#
#   ggplot(dt, aes(x = median, y =residuals)) +
#     geom_boxplot() +
#     theme_bw() +
#     xlab(variable) +
#     ggtitle("GQ")
# }
#
# plot_gqtest3 <- function(ModelAudit){
#   variable <- ModelAudit$gqtest$test$variable
#   dt <- data.frame(y = ModelAudit$data[ ,1], variable = ModelAudit$data[,variable])
#   dt <- dt[order(dt$variable), ]
#   dt$residuals <- c(ModelAudit$gqtest$test$residuals1, ModelAudit$gqtest$test$residuals2)
#
#   n <- nrow(dt)
#   n1 <- n%/%2
#   dt$median <- c(rep("< cut-point1", n1), rep("> cut-point2", n-n1) )
#
#   ggplot(dt, aes(x = median, y =residuals)) +
#     stat_summary(
#       fun.data = function(y) {
#         data.frame(y = var(y),
#                    ymin = ((length(y)-1)*var(y))/qchisq(0.025,length(y)-1),
#                    ymax = ((length(y)-1)*var(y))/qchisq(0.975,length(y)-1))
#       },position=position_dodge(width=0.5)) +
#     theme_bw() +
#     ggtitle("Confidence Intervals for Variances") +
#     xlab(variable) +
#     ylab("Variance of residuals") +
#     coord_flip()
# }
#
# plot_gqtest4 <- function(ModelAudit, variable){
#   y <- model.response(model.frame(ModelAudit$model))
#   residuals <- y - predict(ModelAudit$model)
#   dt <- data.frame(variable = ModelAudit$data[ ,variable],
#                    residuals = residuals)
#   med <- median(dt$variable)
#   ggplot(dt, aes(x=variable, y=residuals)) +
#     geom_point() +
#     geom_vline(xintercept = med) +
#     theme_bw() +
#     xlab(variable)
# }
#
# ############### Other plots #############
# plot_res_vs_fit <- function(ModelAudit){
#   .fitted <- .resid <- NULL
#   df <- ModelAudit$augment
#   ggplot(df, aes(x = .fitted, y = .resid)) +
#     geom_point() +
#     geom_smooth(method = "loess", se = FALSE) +
#     xlab("Fitted values") +
#     ylab("Residuals") +
#     ggtitle("Residuals vs Fitted") +
#     theme_bw()
# }


