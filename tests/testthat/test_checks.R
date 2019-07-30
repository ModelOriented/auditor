context("Scores")

source("objects_for_tests.R")

test_that("check_residuals", {
  expect_is(check_residuals(exp_rf), "list")
})
