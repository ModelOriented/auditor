context("plotD3")

source("objects_for_tests.R")

test_that("plotD3_residual", {
  expect_is(plotD3_residual(au_glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3_residual(au_expl_lm, std_residuals = TRUE), "r2d3")
  expect_is(plotD3_residual(au_rf, variable = "x2"), "r2d3")
  expect_is(plotD3_residual(au_rf, au_lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_residual(au_lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_residual(glm_mr, rf_mr))
  expect_error(plotD3_residual(list(1,2,3)))
})

test_that("plotD3_autocorrelation", {
  expect_is(plotD3_autocorrelation(au_rf, au_lm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3_autocorrelation(au_lm, variable = "x2"), "r2d3")
  expect_is(plotD3_autocorrelation(au_rf, au_lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_is(plotD3_autocorrelation(au_rf, au_lm, smooth = TRUE, point_count = 5), "r2d3")
  expect_error(plotD3_autocorrelation(au_lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_autocorrelation(glm_mr, rf_mr))
  expect_error(plotD3_autocorrelation(list(1,2,3)))
})

test_that("plotD3_prediction", {
  expect_is(plotD3_prediction(au_glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3_prediction(au_expl_lm), "r2d3")
  expect_is(plotD3_prediction(au_rf, variable = "x2"), "r2d3")
  expect_is(plotD3_prediction(au_rf, au_lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_prediction(au_lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_prediction(glm_mr, rf_mr))
  expect_error(plotD3_prediction(list(1,2,3)))
})

test_that("plotD3_scalelocation", {
  expect_is(plotD3_scalelocation(au_glm, smooth = TRUE, point_count = 5), "r2d3")
  expect_is(plotD3_scalelocation(au_expl_lm, peaks = TRUE), "r2d3")
  expect_is(plotD3_scalelocation(au_rf, variable = "x2"), "r2d3")
  expect_is(plotD3_scalelocation(au_rf, au_lm, single_plot = FALSE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_scalelocation(au_lm, points = FALSE, smooth = FALSE))
  expect_error(plotD3_scalelocation(glm_mr, rf_mr))
  expect_error(plotD3_scalelocation(list(1,2,3)))
})

test_that("plotD3_cooksdistance", {
  expect_is(plotD3_cooksdistance(au_glm, cd_lm), "r2d3")
  expect_is(plotD3_cooksdistance(au_rf, au_lm, single_plot = FALSE), "r2d3")
  expect_is(plotD3_cooksdistance(au_rf, au_lm, single_plot = TRUE, scale_plot = TRUE), "r2d3")
  expect_error(plotD3_cooksdistance(list(1,2,3)))
})

test_that("plotD3_lift", {
  expect_is(plotD3_lift(au_class_glm), "r2d3")
  expect_is(plotD3_lift(au_class_glm, au_class_glm2, scale_plot = TRUE), "r2d3")
})

test_that("plotD3_rec", {
  expect_is(plotD3_rec(au_glm), "r2d3")
  expect_is(plotD3_rec(au_glm, au_lm, scale_plot = TRUE), "r2d3")
})

test_that("plotD3_acf", {
  expect_is(plotD3_acf(au_glm), "r2d3")
  expect_is(plotD3_acf(au_glm, au_lm, scale_plot = TRUE), "r2d3")
  expect_is(plotD3_acf(au_glm, au_lm, scale_plot = TRUE, alpha = 0.1), "r2d3")
})

test_that("plotD3_halfnormal", {
  expect_is(plotD3_halfnormal(au_glm), "r2d3")
  expect_is(plotD3_halfnormal(au_glm, au_lm, scale_plot = TRUE), "r2d3")
})
