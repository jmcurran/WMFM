testthat::test_that("rescoreWmfmRepeatedRunsWithOpposingMethod adds prefixed columns", {
  fakeObj = list(
    runsDf = data.frame(
      runId = 1,
      explanationText = "test",
      formula = "y ~ x",
      equationsText = "y = 1 + x",
      interactionTerms = "",
      interactionMinPValue = NA_real_,
      interactionAlpha = 0.05,
      hasError = FALSE,
      stringsAsFactors = FALSE
    ),
    primaryScoringMethod = "deterministic"
  )

  fakeChat = structure(
    list(
      chat = function(prompt) {
        "{}"
      }
    ),
    class = "ProviderFake"
  )

  testthat::local_mocked_bindings(
    scoreWmfmRunWithLlm = function(runRecord, chat, useCache = FALSE, verbose = FALSE) {
      runRecord$overallScore = 1.6
      runRecord$overallPass = TRUE
      runRecord$effectDirectionCorrect = 2L
      runRecord$llmScored = TRUE
      runRecord$llmScoringSummary = "ok"
      runRecord
    }
  )

  out = rescoreWmfmRepeatedRunsWithOpposingMethod(fakeObj, scoringChat = fakeChat, showProgress = FALSE)

  testthat::expect_true("llm_overallScore" %in% names(out$runsDf))
  testthat::expect_identical(out$opposingScoringMethod, "llm")
})
