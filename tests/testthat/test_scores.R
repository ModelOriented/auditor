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
  expect_is(score(exp_glm, "gini"), "auditor_score")
  expect_is(score(exp_glm, "one_minus_gini"), "auditor_score")
  expect_is(score(exp_lm, "r2"), "auditor_score")
  expect_is(score(exp_lm, "cooksdistance"), "numeric")
})

test_that("score_acc", {
  expect_is(score(exp_glm, "acc", data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
  expect_is(score_acc(exp_glm, data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
})

test_that("score_recall", {
  expect_is(score(exp_glm, "recall", data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
  expect_is(score_recall(exp_glm, data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
})

test_that("score_one_minus_precision", {
  expect_is(score(exp_glm, "one_minus_precision", data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
  expect_is(score_one_minus_precision(exp_glm, data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
})

test_that("score_one_minus_specificity", {
  expect_is(score(exp_glm, "one_minus_specificity", data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
  expect_is(score_one_minus_specificity(exp_glm, data = artifficial_classif_2[1:100,], cutoff = 0.3), "auditor_score")
})


test_that("model_performance", {
  expect_is(model_performance(exp_glm, score = c("f1","acc","auc","precision","mse"),
                              variable = "wrongWariableShouldWork", cutoff = 0.3), "auditor_model_performance")
})


test_that("score_auc", {
  equals(score_auc(exp_glm, data = artifficial_classif_2[1:100,], y = artifficial_classif_2$y[1:100]), 0.5664392)
})


test_that("score_auprc", {
  equals(score_auprc(exp_glm), 0.5561686)
})
