context("plotD3")

source("objects_for_tests.R")

test_that("plotD3_residual", {
  expect_is(plotD3(mr_rf, type = "residual", variable = "x2"), "r2d3")
  expect_is(plotD3_residual(mr_glm, smooth = TRUE, point_count = 5, variable = "x2"), "r2d3")
  expect_is(plotD3_residual(mr_glm, std_residuals = TRUE), "r2d3")
  expect_is(plotD3_residual(mr_rf, variable = "x2"), "r2d3")
  expect_is(plotD3_residual(mr_rf, mr_glm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_residual(mr_lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_residual(list(1,2,3)))
})

test_that("plotD3_autocorrelation", {
  expect_is(plotD3(mr_glm, type = "autocorrelation"), "r2d3")
  expect_is(plotD3_autocorrelation(mr_rf, mr_glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3_autocorrelation(mr_glm), "r2d3")
  expect_is(plotD3_autocorrelation(mr_rf, mr_glm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_is(plotD3_autocorrelation(mr_rf, mr_glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_error(plotD3_autocorrelation(mr_glm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_autocorrelation(list(1,2,3)))
})

test_that("plotD3_prediction", {
  expect_is(plotD3(mr_glm, type = "prediction"), "r2d3")
  expect_is(plotD3_prediction(mr_glm, smooth = TRUE, point_count = 5, variable = "x2"), "r2d3")
  expect_is(plotD3_prediction(mr_rf), "r2d3")
  expect_is(plotD3_prediction(mr_rf, mr_glm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_prediction(au_lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_prediction(glm_mr, rf_mr))
  expect_error(plotD3_prediction(list(1,2,3)))
})

test_that("plotD3_scalelocation", {
  expect_is(plotD3(mr_rf, type = "scalelocation"), "r2d3")
  expect_is(plotD3_scalelocation(mr_glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3_scalelocation(mr_glm, peaks = TRUE), "r2d3")
  expect_is(plotD3_scalelocation(mr_rf), "r2d3")
  expect_is(plotD3_scalelocation(mr_rf, mr_glm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_scalelocation(mr_glm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_scalelocation(list(1,2,3)))
})

test_that("plotD3_cooksdistance", {
  expect_is(plotD3(cd_lm, type = "cooksdistance"), "r2d3")
  expect_is(plotD3_cooksdistance(cd_lm, cd_rf), "r2d3")
  expect_is(plotD3_cooksdistance(cd_rf, cd_lm, single_plot = FALSE), "r2d3")
  expect_is(plotD3_cooksdistance(cd_rf, cd_lm, single_plot = TRUE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_cooksdistance(list(1,2,3)))
})

test_that("plotD3_lift", {
  expect_is(plotD3(ev_glm, type = "lift"), "r2d3")
  expect_is(plotD3_lift(ev_glm), "r2d3")
  expect_is(plotD3_lift(ev_glm, ev_rf, scale_plot = TRUE), "r2d3")
})

test_that("plotD3_rec", {
  expect_is(plotD3(mr_glm, type = "rec"), "r2d3")
  expect_is(plotD3_rec(mr_glm), "r2d3")
  expect_is(plotD3_rec(mr_glm, mr_rf, scale_plot = TRUE), "r2d3")
})

test_that("plotD3_acf", {
  expect_is(plotD3(mr_glm, type = "acf"), "r2d3")
  expect_is(plotD3_acf(mr_glm), "r2d3")
  expect_is(plotD3_acf(mr_glm, mr_rf, scale_plot = TRUE), "r2d3")
  expect_is(plotD3_acf(mr_glm, mr_rf, scale_plot = TRUE, alpha = 0.1), "r2d3")
})

test_that("plotD3_halfnormal", {
  expect_is(plotD3(hn_glm, type = "halfnormal"), "r2d3")
  expect_is(plotD3_halfnormal(hn_glm), "r2d3")
  expect_is(plotD3_halfnormal(hn_glm, hn_rf, scale_plot = TRUE), "r2d3")
})

test_that("plotD3_roc", {
  expect_is(plotD3(ev_glm, type = "roc"), "r2d3")
  expect_is(plotD3_roc(ev_glm), "r2d3")
  expect_is(plotD3_roc(ev_glm, ev_rf, scale_plot = TRUE), "r2d3")
})

test_that("plotD3_rroc", {
  expect_is(plotD3(mr_glm, type = "rroc"), "r2d3")
  expect_is(plotD3_rroc(mr_glm), "r2d3")
  expect_is(plotD3_rroc(mr_glm, mr_rf, scale_plot = TRUE), "r2d3")
})
