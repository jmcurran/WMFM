makeValidParsedScores = function() {
  list(
    effectDirectionCorrect = 2,
    effectScaleAppropriate = 2,
    referenceGroupHandledCorrectly = 1,
    interactionCoverageAdequate = 2,
    interactionSubstantiveCorrect = 2,
    uncertaintyHandlingAppropriate = 1,
    inferentialRegisterAppropriate = 1,
    mainEffectCoverageAdequate = 2,
    referenceGroupCoverageAdequate = 1,
    clarityAdequate = 2,
    numericExpressionAdequate = 2,
    comparisonStructureClear = 2,
    fatalFlawDetected = FALSE,
    factualScore = 1.8,
    inferenceScore = 1.4,
    completenessScore = 1.7,
    clarityScore = 1.9,
    calibrationScore = 1.3,
    overallScore = 1.6,
    overallPass = TRUE,
    llmScoringSummary = "Mostly correct.",
    fieldReasons = list(
      effectDirectionCorrect = "ok",
      effectScaleAppropriate = "ok",
      referenceGroupHandledCorrectly = "ok",
      interactionCoverageAdequate = "ok",
      interactionSubstantiveCorrect = "ok",
      uncertaintyHandlingAppropriate = "ok",
      inferentialRegisterAppropriate = "ok",
      mainEffectCoverageAdequate = "ok",
      referenceGroupCoverageAdequate = "ok",
      clarityAdequate = "ok",
      numericExpressionAdequate = "ok",
      comparisonStructureClear = "ok",
      fatalFlawDetected = "ok"
    )
  )
}

testthat::test_that("validateWmfmParsedScores validates good input", {
  out = validateWmfmParsedScores(makeValidParsedScores())

  testthat::expect_identical(out$effectDirectionCorrect, 2L)
  testthat::expect_identical(out$fatalFlawDetected, FALSE)
  testthat::expect_true(is.list(out$fieldReasons))
})

testthat::test_that("validateWmfmParsedScores rejects bad rubric values", {
  x = makeValidParsedScores()
  x$effectDirectionCorrect = 4

  testthat::expect_error(
    validateWmfmParsedScores(x),
    "effectDirectionCorrect"
  )
})

testthat::test_that("validateWmfmParsedScores rejects missing reason fields", {
  x = makeValidParsedScores()
  x$fieldReasons$clarityAdequate = NULL

  testthat::expect_error(
    validateWmfmParsedScores(x),
    "fieldReasons"
  )
})
