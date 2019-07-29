context("plots")

source("objects_for_tests.R")

test_that("plot_acf", {
  expect_is(plot_acf(rf_mr, variable = "x2"), "gg")
  expect_is(plot_acf(rf_mr), "gg")
})

test_that("plot_autocorrelation", {
  expect_is(plot_autocorrelation(rf_mr), "gg")
  expect_is(plot_autocorrelation(rf_mr, variable = ""), "gg")
  expect_is(plot_autocorrelation(rf_mr, variable = "x2"), "gg")
  expect_is(plot_autocorrelation(rf_mr, smooth = TRUE), "gg")
  expect_is(plot_autocorrelation(rf_mr, glm_mr), "gg")
})

test_that("plot_cooksdistance", {
  expect_is(plot_cooksdistance(au_lm), "gg")
  expect_is(plot_cooksdistance(au_lm, au_rf), "gg")
})

test_that("plot_prediction", {
  expect_is(plot_prediction(rf_mr, smooth = TRUE), "gg")
  expect_is(plot_prediction(rf_mr), "gg")
  expect_is(plot_prediction(rf_mr, variable = ""), "gg")
  expect_is(plot_prediction(rf_mr, abline = TRUE), "gg")
})

test_that("plot_residual", {
  expect_is(plot_residual(glm_mr), "gg")
  expect_is(plot_residual(glm_mr, variable = ""), "gg")
  expect_is(plot_residual(glm_mr, variable = "x2"), "gg")
  expect_is(plot_residual(glm_mr, std_residuals = TRUE, smooth = TRUE, nlabel = 5), "gg")
})

test_that("plot_residual_boxplot", {
  expect_is(plot_residual_boxplot(rf_mr, au_lm), "gg")
})

test_that("plot_residual_density", {
  expect_is(plot_residual_density(rf_mr), "gg")
  expect_is(plot_residual_density(rf_mr, split = TRUE), "gg")
  expect_is(plot_residual_density(rf_mr, variable = "x3", split = TRUE), "gg")
  expect_is(plot_residual_density(rf_mr, variable = ""), "gg")
  expect_is(plot_residual_density(rf_mr, glm_mr, variable = ""), "gg")
  expect_is(plot_residual_density(rf_mr, glm_mr, variable = ""), "gg")
})

test_that("plot_scalelocation", {
  expect_is(plot_scalelocation(rf_mr), "gg")
  expect_is(plot_scalelocation(rf_mr, glm_mr), "gg")
  expect_is(plot_scalelocation(rf_mr, variable = ""), "gg")
  expect_is(plot_scalelocation(rf_mr, smooth = TRUE), "gg")
  expect_is(plot_scalelocation(rf_mr, peaks = TRUE), "gg")
})

test_that("plot_tsecdf", {
  expect_is(plot_tsecdf(rf_mr, glm_mr), "gg")
  expect_is(plot_tsecdf(rf_mr, glm_mr, residuals = FALSE), "gg")
  expect_is(plot_tsecdf(rf_mr, glm_mr, reverse_y = TRUE), "gg")
  expect_is(plot_tsecdf(rf_mr, glm_mr, outliers = 2), "gg")
})

test_that("plot_halfnormal", {
  expect_is(plot_halfnormal(au_glm), "gg")
  expect_is(plot_halfnormal(au_lm, quantiles = TRUE), "gg")
  expect_is(plot_halfnormal(au_class_rf), "gg")
})

test_that("plot_lift", {
  expect_is(plot_lift(au_class_glm2), "gg")
  expect_is(plot_lift(au_class_glm, au_class_glm), "gg")
})

test_that("plot_roc", {
  expect_is(plot_roc(au_class_glm), "gg")
  expect_is(plot_roc(au_class_glm, au_class_glm2), "gg")
  expect_is(plot_roc(au_class_glm, au_class_glm2, nlabels = 4), "gg")
})

test_that("plot_rroc", {
  expect_is(plot_rroc(rf_mr), "gg")
  expect_is(plot_rroc(rf_mr, glm_mr), "gg")
})

test_that("plot_rec", {
  expect_is(plot_rec(rf_mr), "gg")
  expect_is(plot_rec(rf_mr, glm_mr), "gg")
})

test_that("plot_radar", {
  new_score1 <- function(object) sum(sqrt(abs(object$residuals)))
  new_score2 <- function(object) sum(sqrt(abs(object$residuals)) + 1)
  lm_mp <- model_performance(au_lm,
                            score = c("mae", "mse", "rec", "rroc"),
                            new_score = list(n1 = new_score1, n2 = new_score2))
  lm_mp2 <- model_performance(au_rf,
                             score = c("mae", "mse", "rec", "rroc"),
                             new_score = list(n1 = new_score1, n2 = new_score2))
  expect_is(plot_radar(lm_mp, lm_mp2), "gg")
})


test_that("plotModelCorrelation", {
  expect_is(plot_correlation(rf_mr, glm_mr), "gtable")
  expect_is(plot_correlation(rf_mr, glm_mr, values = "res"), "gtable")
})


test_that("plot", {
  expect_is(plot(rf_mr, type="acf"), "gg")
  expect_is(plot(rf_mr, type="autocorrelation", score = TRUE), "gg")
  expect_is(plot(au_lm, type="cooksdistance", print=FALSE), "gg")
  expect_is(plot(au_lm, au_rf, type="radar"), "gg")
  expect_is(plot(rf_mr, type="tsecdf"), "gg")
  expect_is(plot(rf_mr, glm_mr, type="pca"), "gg")
  expect_is(plot(rf_mr, type="residual_density"), "gg")
  expect_is(plot(au_class_glm2, type="lift"), "gg")
  expect_is(plot(rf_mr, glm_mr, type="correlation"), "gtable")
  expect_is(plot(rf_mr, glm_mr, type="correlation", values = "res"), "gtable")
  expect_is(plot(rf_mr, type="prediction"), "gg")
  expect_is(plot(rf_mr, type="residual"), "gg")
  expect_is(plot(rf_mr, type="residual_boxplot"), "gg")
  expect_is(plot(rf_mr, type="scalelocation"), "gg")
  expect_is(plot(au_glm, type = "halfnormal"), "gg")
  expect_is(plot(au_class_glm, au_class_glm2, type="roc"), "gg")
  expect_is(plot(rf_mr, type="rroc"), "gg")
  expect_is(plot(rf_mr, type="rec"), "gg")
  expect_error(plot(rf_mr, type="wrongType"))
})

test_that("multiple plots on grid", {
  expect_is(plot(rf_mr, type=c("prediction", "residual"), grid = TRUE), "gtable")
})

test_that("plot, grid equals FALSE", {
  expect_is(plot(rf_mr, type = c("prediction", "residual"), grid = FALSE, ask = FALSE), "auditor_plot_list")
})

test_that("plot type is not provided", {
  expect_is(plot(cd_lm), "gg")
  expect_is(plot(mp_lm), "gg")
  expect_is(plot(mf_lm), "gg")
})

test_that("theme drwhy colors generates rigth length vectors", {
  z <- 1:9
  all.equal(unlist(lapply(z, function(x) length(theme_drwhy_colors(x)))), z)
})
