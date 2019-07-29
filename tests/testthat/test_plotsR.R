context("plots")

source("objects_for_tests.R")

test_that("plot_acf", {
  expect_is(plot_acf(au_lm, variable = "x2"), "gg")
  expect_is(plot_acf(au_rf), "gg")
})

test_that("plot_autocorrelation", {
  expect_is(plot_autocorrelation(au_lm), "gg")
  expect_is(plot_autocorrelation(au_lm, variable = ""), "gg")
  expect_is(plot_autocorrelation(au_lm, variable = "x2"), "gg")
  expect_is(plot_autocorrelation(au_lm, smooth = TRUE), "gg")
  expect_is(plot_autocorrelation(au_rf, au_lm), "gg")
})

test_that("plot_cooksdistance", {
  expect_is(plot_cooksdistance(au_lm), "gg")
  expect_is(plot_cooksdistance(au_lm, au_rf), "gg")
})

test_that("plot_prediction", {
  expect_is(plot_prediction(au_rf, smooth = TRUE), "gg")
  expect_is(plot_prediction(au_rf, variable = "x2"), "gg")
  expect_is(plot_prediction(au_rf, variable = ""), "gg")
  expect_is(plot_prediction(au_rf, abline = TRUE), "gg")
})

test_that("plot_residual", {
  expect_is(plot_residual(au_glm), "gg")
  expect_is(plot_residual(au_glm, variable = ""), "gg")
  expect_is(plot_residual(au_glm, variable = "x2"), "gg")
  expect_is(plot_residual(au_glm, std_residuals = TRUE, smooth = TRUE, nlabel = 5), "gg")
  expect_is(plot_residual(au_expl_lm), "gg")
})

test_that("plot_residual_boxplot", {
  expect_is(plot_residual_boxplot(au_rf, au_lm), "gg")
})

test_that("plot_residual_density", {
  expect_is(plot_residual_density(au_lm), "gg")
  expect_is(plot_residual_density(au_lm, split = TRUE), "gg")
  expect_is(plot_residual_density(au_lm, variable = "x3", split = TRUE), "gg")
  expect_is(plot_residual_density(au_lm, variable = ""), "gg")
  expect_is(plot_residual_density(au_lm, au_rf, variable = ""), "gg")
  expect_is(plot_residual_density(au_lm, au_rf, variable = ""), "gg")
})

test_that("plot_scalelocation", {
  expect_is(plot_scalelocation(au_glm), "gg")
  expect_is(plot_scalelocation(au_lm, au_rf), "gg")
  expect_is(plot_scalelocation(model_residual(au_glm)), "gg")
  expect_is(plot_scalelocation(au_glm, variable = ""), "gg")
  expect_is(plot_scalelocation(au_glm, smooth = TRUE), "gg")
  expect_is(plot_scalelocation(au_glm, peaks = TRUE), "gg")
  expect_is(plot_scalelocation(au_rf, variable = "x2"), "gg")
})

test_that("plot_tsecdf", {
  expect_is(plot_tsecdf(au_lm, au_rf), "gg")
  expect_is(plot_tsecdf(au_lm, au_rf, residuals = FALSE), "gg")
  expect_is(plot_tsecdf(au_lm, au_rf, reverse_y = TRUE), "gg")
  expect_is(plot_tsecdf(au_lm, au_rf, outliers = 2), "gg")
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
  expect_is(plot_rroc(au_glm), "gg")
  expect_is(plot_rroc(au_glm, au_rf), "gg")
})

test_that("plot_rec", {
  expect_is(plot_rec(au_glm), "gg")
  expect_is(plot_rec(model_residual(au_glm)), "gg")
  expect_is(plot_rec(au_glm, au_rf), "gg")
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
  expect_is(plot_correlation(au_glm, au_rf), "gtable")
  expect_is(plot_correlation(au_glm, au_rf, values = "res"), "gtable")
})


test_that("plot", {
  expect_is(plot(au_lm, type="acf"), "gg")
  expect_is(plot(au_lm, type="autocorrelation", score = TRUE), "gg")
  expect_is(plot(au_lm, type="cooksdistance", print=FALSE), "gg")
  expect_is(plot(au_lm, au_rf, type="radar"), "gg")
  expect_is(plot(au_lm, au_rf, type="tsecdf"), "gg")
  expect_is(plot(au_glm, au_rf, type="pca"), "gg")
  expect_is(plot(au_lm, type="residual_density"), "gg")
  expect_is(plot(au_class_glm2, type="lift"), "gg")
  expect_is(plot(au_glm, au_rf, type="correlation"), "gtable")
  expect_is(plot(au_glm, au_rf, type="correlation", values = "res"), "gtable")
  expect_is(plot(au_lm, au_rf, type="prediction"), "gg")
  expect_is(plot(au_lm, type="residual"), "gg")
  expect_is(plot(au_lm, type="residual_boxplot"), "gg")
  expect_is(plot(au_lm, type="scalelocation"), "gg")
  expect_is(plot(au_glm, type = "halfnormal"), "gg")
  expect_is(plot(au_class_glm, au_class_glm2, type="roc"), "gg")
  expect_is(plot(au_glm, au_rf, type="rroc"), "gg")
  expect_is(plot(au_glm, au_rf, type="rec"), "gg")
  expect_error(plot(au_lm, type="wrongType"))
})

test_that("multiple plots on grid", {
  expect_is(plot(au_lm, au_rf, type=c("prediction", "residual"), grid = TRUE), "gtable")
})

test_that("plot, grid equals FALSE", {
  expect_is(plot(au_lm, au_rf, type = c("prediction", "residual"), grid = FALSE, ask = FALSE), "auditor_plot_list")
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
