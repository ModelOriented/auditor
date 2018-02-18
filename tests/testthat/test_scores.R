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
  expect_is(scoreCook(au.lm), "numeric")
  expect_is(scoreCook(au.rf), "numeric")
})


test_that("scoreHalfNormal", {
  expect_is(scoreHalfNormal(au.lm), "numeric")
})
