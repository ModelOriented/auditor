context("Scores")

source("objects_for_tests.R")

test_that("score_peak", {
  expect_is(score_peak(exp_glm, "x2"), "auditor_score")
  expect_is(score_peak(exp_rf), "auditor_score")
})

test_that("score_dw", {
  expect_is(score_dw(exp_lm), "auditor_score")
  expect_is(score_dw(exp_rf, "x2"), "auditor_score")
})

test_that("score_runs", {
  expect_is(score_runs(exp_lm), "auditor_score")
  expect_is(score_runs(exp_glm, "x2"), "auditor_score")
})

test_that("score_cooksdistance", {
  expect_is(score_cooksdistance(exp_lm), "numeric")
  expect_is(score_cooksdistance(exp_rf), "numeric")
})


test_that("score_halfnormal", {
  expect_is(score_halfnormal(exp_lm), "auditor_score")
})

test_that("score", {
  expect_is(score(exp_lm, "peak", variable="x2"), "auditor_score")
  expect_is(score(exp_lm, "dw", variable="x2"), "auditor_score")
  expect_is(score(exp_lm, "runs", variable="x2"), "auditor_score")
  expect_is(score(exp_lm, "halfnormal"), "auditor_score")
  expect_is(score(exp_lm, "rmse"), "auditor_score")
  expect_is(score(exp_glm, "auc"), "auditor_score")
  expect_is(score(exp_lm, "cooksdistance"), "numeric")
})




