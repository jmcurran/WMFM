makeRunRecordForApply = function() {
  list(
    runId = 1L,
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    formula = "Exam ~ Attend + Test",
    equationsText = "Exam = 6.62 + 3.52 * Test",
    explanationText = "A clear explanation.",
    interactionTerms = "",
    hasInteractionTerms = FALSE,
    nInteractionTerms = 0L,
    interactionMinPValue = NA_real_,
    interactionAlpha = 0.05,
    effectDirectionCorrect = NA_integer_,
    effectScaleAppropriate = NA_integer_,
    referenceGroupHandledCorrectly = NA_integer_,
    interactionCoverageAdequate = NA_integer_,
    interactionSubstantiveCorrect = NA_integer_,
    uncertaintyHandlingAppropriate = NA_integer_,
    inferentialRegisterAppropriate = NA_integer_,
    mainEffectCoverageAdequate = NA_integer_,
    referenceGroupCoverageAdequate = NA_integer_,
    clarityAdequate = NA_integer_,
    numericExpressionAdequate = NA_integer_,
    comparisonStructureClear = NA_integer_,
    fatalFlawDetected = NA,
    factualScore = NA_real_,
    inferenceScore = NA_real_,
    completenessScore = NA_real_,
    clarityScore = NA_real_,
    calibrationScore = NA_real_,
    overallScore = NA_real_,
    overallPass = NA
  )
}

testthat::test_that("applyWmfmLlmScoresToRecord writes validated fields", {
  parsed = makeValidParsedScores()
  runRecord = makeRunRecordForApply()

  out = applyWmfmLlmScoresToRecord(
    runRecord = runRecord,
    parsedScores = parsed,
    modelName = "ProviderClaude",
    rawResponse = "{\"ok\":true}"
  )

  testthat::expect_identical(out$effectDirectionCorrect, 2L)
  testthat::expect_identical(out$overallPass, TRUE)
  testthat::expect_identical(out$llmScored, TRUE)
  testthat::expect_true(is.character(out$llmFieldReasons))
})
