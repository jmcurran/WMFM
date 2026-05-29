testthat::test_that("Stage 25.3 earthquake GLM follow-up resolves explicit natural-language assignments", {
  earthquakeData = data.frame(
    Freq = c(28, 18, 10, 5, 23, 14, 7, 3),
    Magnitude = c(3, 4, 5, 6, 3, 4, 5, 6),
    Locn = factor(c("SC", "SC", "SC", "SC", "WA", "WA", "WA", "WA"), levels = c("SC", "WA"))
  )
  model = stats::glm(Freq ~ Magnitude + Locn, data = earthquakeData, family = stats::poisson())

  out = computeGlmModelQuestionPrediction(
    model,
    "What would the expected earthquake frequency be for Magnitude = 3 in Locn = WA with a confidence interval and prediction interval?",
    allowMissingPredictorCompletion = FALSE
  )

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$resolvedPredictorValues$Magnitude, 3)
  testthat::expect_identical(out$resolvedPredictorValues$Locn, "WA")
  testthat::expect_true(is.list(out$confidenceInterval))
  testthat::expect_match(out$predictionIntervalUnsupportedReason, "not currently supported", fixed = TRUE)
})

testthat::test_that("Stage 25.3 assignment extraction separates for/in predictor clauses", {
  outIn = extractPredictionAssignmentPairs("For Magnitude = 3 in Locn = WA, what earthquake frequency would I expect?")
  testthat::expect_identical(outIn$Magnitude, "3")
  testthat::expect_identical(outIn$Locn, "WA")

  outAnd = extractPredictionAssignmentPairs("For Magnitude = 3 and Locn = WA, what earthquake frequency would I expect?")
  testthat::expect_identical(outAnd$Magnitude, "3")
  testthat::expect_identical(outAnd$Locn, "WA")
})
