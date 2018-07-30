context("Scores")

source("objects_for_tests.R")

test_that("scoreGQ", {
  expect_is(scoreGQ(au.lm, "income"), "scoreAudit")
  expect_is(scoreGQ(au.rf), "scoreAudit")
})

test_that("scoreDW", {
  expect_is(scoreDW(au.lm), "scoreAudit")
  expect_is(scoreDW(au.rf, "Prewt"), "scoreAudit")
})


test_that("scoreRuns", {
  expect_is(scoreRuns(au.lm), "scoreAudit")
  expect_is(scoreRuns(au.glm, "Prewt"), "scoreAudit")
})

test_that("scoreCook", {
  expect_is(scoreCooksDistance(au.lm), "numeric")
  expect_is(scoreCooksDistance(au.rf), "numeric")
})


test_that("scoreHalfNormal", {
  expect_is(scoreHalfNormal(au.lm), "scoreAudit")
})

test_that("score", {
  expect_is(score(au.lm, "GQ", variable="income"), "scoreAudit")
  expect_is(score(au.lm, "DW", variable="income"), "scoreAudit")
  expect_is(score(au.lm, "Runs", variable="income"), "scoreAudit")
  expect_is(score(au.lm, "HalfNormal"), "scoreAudit")
  expect_is(score(au.lm, "RMSE"), "scoreAudit")
  expect_is(score(au.class.glm, "ROC"), "scoreAudit")
  expect_is(score(au.lm, "CooksDistance"), "numeric")
  expect_error(score(model.lm, type = "Runs"))
  expect_error(score(au.lm,"wrongScore"))
})




