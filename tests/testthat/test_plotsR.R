context("plots")

source("objects_for_tests.R")

test_that("plot_acf", {
  expect_is(plot_acf(mr_rf, variable = "x2"), "gg")
  expect_is(plot_acf(mr_rf, variable = "_y_"), "gg")
  expect_is(plot_acf(mr_rf, variable = "_y_hat_"), "gg")
  expect_is(plot_acf(mr_rf), "gg")
})

test_that("plot_autocorrelation", {
  expect_is(plot_autocorrelation(mr_rf), "gg")
  expect_is(plot_autocorrelation(mr_rf, variable = "_y_"), "gg")
  expect_is(plot_autocorrelation(mr_rf, variable = "_y_hat_"), "gg")
  expect_is(plot_autocorrelation(mr_rf, variable = "x1"), "gg")
  expect_is(plot_autocorrelation(mr_rf, smooth = TRUE), "gg")
  expect_is(plot_autocorrelation(mr_rf, mr_glm), "gg")
})

test_that("plot_cooksdistance", {
  expect_is(plot_cooksdistance(cd_lm), "gg")
  expect_is(plot_cooksdistance(cd_lm, cd_rf), "gg")
})

test_that("plot_prediction", {
  expect_is(plot_prediction(mr_rf, smooth = TRUE), "gg")
  expect_is(plot_prediction(mr_rf), "gg")
  expect_is(plot_prediction(mr_rf, variable = "_y_hat_"), "gg")
  expect_is(plot_prediction(mr_rf, abline = TRUE), "gg")
})

test_that("plot_residual", {
  expect_is(plot_residual(mr_glm), "gg")
  expect_is(plot_residual(mr_glm, variable = NULL), "gg")
  expect_is(plot_residual(mr_glm, variable = "_y_hat_"), "gg")
  expect_is(plot_residual(mr_glm, variable = "x2"), "gg")
  expect_is(plot_residual(mr_glm, std_residuals = TRUE, smooth = TRUE, nlabel = 5), "gg")
})

test_that("plot_residual_boxplot", {
  expect_is(plot_residual_boxplot(mr_rf, mr_glm), "gg")
})

test_that("plot_residual_density", {
  expect_is(plot_residual_density(mr_rf), "gg")
  expect_is(plot_residual_density(mr_rf, variable = NULL), "gg")
  expect_is(plot_residual_density(mr_rf, variable = "x2"), "gg")
  expect_is(plot_residual_density(mr_rf, variable = ""), "gg")
  expect_is(plot_residual_density(mr_rf, variable = "_y_"), "gg")
  expect_is(plot_residual_density(mr_rf, variable = "_y_hat_"), "gg")
  expect_is(plot_residual_density(mr_rf, mr_glm, variable = "_y_hat_"), "gg")
})

test_that("plot_scalelocation", {
  expect_is(plot_scalelocation(mr_rf), "gg")
  expect_is(plot_scalelocation(mr_rf, mr_glm, variable = NULL), "gg")
  expect_is(plot_scalelocation(mr_rf, variable = "_y_hat_"), "gg")
  expect_is(plot_scalelocation(mr_rf, variable = "x2"), "gg")
  expect_is(plot_scalelocation(mr_rf, smooth = TRUE), "gg")
  expect_is(plot_scalelocation(mr_rf, peaks = TRUE), "gg")
})

test_that("plot_tsecdf", {
  expect_is(plot_tsecdf(mr_rf, mr_glm), "gg")
  expect_is(plot_tsecdf(mr_rf, mr_glm, residuals = FALSE), "gg")
  expect_is(plot_tsecdf(mr_rf, mr_glm, reverse_y = TRUE), "gg")
  expect_is(plot_tsecdf(mr_rf, mr_glm, outliers = 2), "gg")
})

test_that("plot_halfnormal", {
  expect_is(plot_halfnormal(hn_glm), "gg")
  expect_is(plot_halfnormal(hn_glm, quantiles = TRUE), "gg")
  expect_is(plot_halfnormal(hn_rf), "gg")
})

test_that("plot_lift", {
  expect_is(plot_lift(ev_rf, ev_glm), "gg")
})

test_that("plot_roc", {
  expect_is(plot_roc(ev_rf, ev_glm), "gg")
  expect_is(plot_roc(ev_rf, ev_glm, nlabel = 4), "gg")
})

test_that("plot_rroc", {
  expect_is(plot_rroc(mr_rf, mr_glm), "gg")
})

test_that("plot_rec", {
  expect_is(plot_rec(mr_rf, mr_glm), "gg")
})

test_that("plot_radar", {
  expect_is(plot_radar(mp_lm, mp_rf, verbose = FALSE), "gg")
})


test_that("plotModelCorrelation", {
  expect_is(plot_correlation(mr_rf, mr_glm), "gtable")
  expect_is(plot_correlation(mr_rf, mr_glm, values = "res"), "gtable")
})


test_that("plot", {
  expect_is(plot(mr_rf, type="acf"), "gg")
  expect_is(plot(mr_rf, type="autocorrelation", score = TRUE), "gg")
  expect_is(plot(cd_lm, type="cooksdistance"), "gg")
  expect_is(plot(mp_lm, mp_lm, verbose = FALSE, type = "radar"), "gg")
  expect_is(plot(mr_rf, type="tsecdf"), "gg")
  expect_is(plot(mr_rf, mr_glm, type="pca"), "gg")
  expect_is(plot(mr_rf, type="residual_density"), "gg")
  expect_is(plot(ev_glm, type="lift"), "gg")
  expect_is(plot(mr_rf, mr_glm, type="correlation"), "gtable")
  expect_is(plot(mr_rf, mr_glm, type="correlation", values = "res"), "gtable")
  expect_is(plot(mr_rf, type="prediction"), "gg")
  expect_is(plot(mr_rf, type="residual"), "gg")
  expect_is(plot(mr_rf, type="residual_boxplot"), "gg")
  expect_is(plot(mr_rf, type="scalelocation"), "gg")
  expect_is(plot(hn_glm, type = "halfnormal"), "gg")
  expect_is(plot(ev_glm, ev_rf, type="roc"), "gg")
  expect_is(plot(mr_rf, type="rroc"), "gg")
  expect_is(plot(mr_rf, type="rec"), "gg")
  expect_error(plot(mr_rf, type="wrongType"))
})


test_that("multiple plots on grid", {
  expect_is(plot(mr_rf, type=c("prediction", "residual"), grid = TRUE), "gtable")
})

test_that("plot, grid equals FALSE", {
  expect_is(plot(mr_rf, type = c("prediction", "residual"), grid = FALSE, ask = FALSE), "auditor_plot_list")
})

test_that("plot type is not provided", {
  expect_is(plot(cd_lm), "gg")
  expect_is(plot(mp_lm), "gg")
  expect_is(plot(mr_glm), "gg")
})

test_that("theme drwhy colors generates rigth length vectors", {
  z <- 1:9
  expect_equal(unlist(lapply(z, function(x) length(theme_drwhy_colors(x)))), z)
})

