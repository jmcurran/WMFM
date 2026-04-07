testthat::test_that("applyWmfmLlmScoresToRecord returns a pure score record", {
  parsed = makeValidParsedScores()

  out = applyWmfmLlmScoresToRecord(
    parsedScores = parsed,
    modelName = "ProviderClaude",
    rawResponse = "{\"ok\":true}"
  )

  testthat::expect_identical(out$effectDirectionCorrect, 2L)
  testthat::expect_identical(out$overallPass, TRUE)
  testthat::expect_identical(out$llmScored, TRUE)
  testthat::expect_identical(out$llmScoringModel, "ProviderClaude")
  testthat::expect_identical(out$llmScoringRaw, "{\"ok\":true}")
  testthat::expect_true(is.character(out$llmFieldReasons))

  testthat::expect_false(any(c(
    "runId",
    "exampleName",
    "formula",
    "equationsText",
    "explanationText"
  ) %in% names(out)))
})
