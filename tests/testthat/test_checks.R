context("Scores")

source("objects_for_tests.R")

test_that("scorePeak", {
  expect_is(check_residuals(au_lm), "list")
})
