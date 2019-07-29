context("Scores")

source("objects_for_tests.R")

test_that("score_peak", {
  expect_is(score_peak(au_lm, "x2"), "score_audit")
  expect_is(score_peak(au_rf), "score_audit")
})

test_that("score_dw", {
  expect_is(score_dw(au_lm), "score_audit")
  expect_is(score_dw(au_rf, "x2"), "score_audit")
})


test_that("score_runs", {
  expect_is(score_runs(au_lm), "score_audit")
  expect_is(score_runs(au_glm, "x2"), "score_audit")
})

test_that("score_cooksdistance", {
  expect_is(score_cooksdistance(au_lm), "numeric")
  expect_is(score_cooksdistance(au_rf), "numeric")
})


test_that("score_halfnormal", {
  expect_is(score_halfnormal(au_lm), "score_audit")
})

test_that("score", {
  expect_is(score(au_lm, "peak", variable="x2"), "score_audit")
  expect_is(score(au_lm, "dw", variable="x2"), "score_audit")
  expect_is(score(au_lm, "runs", variable="x2"), "score_audit")
  expect_is(score(au_lm, "halfnormal"), "score_audit")
  expect_is(score(au_lm, "rmse"), "score_audit")
  expect_is(score(au_class_glm, "auc"), "score_audit")
  expect_is(score(au_lm, "cooksdistance"), "numeric")
  expect_error(score(model_lm, type = "runs"))
  expect_error(score(au_lm,"wrongScore"))
})




