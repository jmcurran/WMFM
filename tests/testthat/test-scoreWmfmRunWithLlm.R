testthat::test_that("scoreWmfmRunWithLlm scores one raw run and returns a pure score record", {
  raw = paste0(
    "{",
    '"effectDirectionCorrect":2,',
    '"effectScaleAppropriate":2,',
    '"referenceGroupHandledCorrectly":1,',
    '"interactionCoverageAdequate":2,',
    '"interactionSubstantiveCorrect":2,',
    '"uncertaintyHandlingAppropriate":1,',
    '"inferentialRegisterAppropriate":1,',
    '"mainEffectCoverageAdequate":2,',
    '"referenceGroupCoverageAdequate":1,',
    '"clarityAdequate":2,',
    '"numericExpressionAdequate":2,',
    '"comparisonStructureClear":2,',
    '"fatalFlawDetected":false,',
    '"factualScore":1.8,',
    '"inferenceScore":1.4,',
    '"completenessScore":1.7,',
    '"clarityScore":1.9,',
    '"calibrationScore":1.3,',
    '"overallScore":90,',
    '"overallPass":true,',
    '"llmScoringSummary":"Mostly correct.",',
    '"fieldReasons":{',
    '"effectDirectionCorrect":"ok",',
    '"effectScaleAppropriate":"ok",',
    '"referenceGroupHandledCorrectly":"ok",',
    '"interactionCoverageAdequate":"ok",',
    '"interactionSubstantiveCorrect":"ok",',
    '"uncertaintyHandlingAppropriate":"ok",',
    '"inferentialRegisterAppropriate":"ok",',
    '"mainEffectCoverageAdequate":"ok",',
    '"referenceGroupCoverageAdequate":"ok",',
    '"clarityAdequate":"ok",',
    '"numericExpressionAdequate":"ok",',
    '"comparisonStructureClear":"ok",',
    '"fatalFlawDetected":"ok"',
    "}",
    "}"
  )

  chat = makeFakeChat(raw)
  out = scoreWmfmRunWithLlm(makeRawRunRecordForScoring(), chat = chat)

  testthat::expect_identical(out$llmScored, TRUE)
  testthat::expect_identical(out$effectDirectionCorrect, 2L)
  testthat::expect_identical(out$llmScoringModel, "ProviderFake")
  testthat::expect_false(any(c("runId", "formula", "explanationText") %in% names(out)))
})


testthat::test_that("scoreWmfmRunWithLlm skips errored runs without provider access", {
  out = scoreWmfmRunWithLlm(
    makeRawRunRecordForScoring(hasError = TRUE),
    chat = makeFakeChat("{}")
  )

  testthat::expect_identical(out$llmScored, FALSE)
  testthat::expect_match(out$llmScoringSummary, "skipped")
})


testthat::test_that("scoreWmfmRunWithLlm cache key reflects adjustment-role context", {
  raw = paste0(
    "{",
    '"effectDirectionCorrect":2,',
    '"effectScaleAppropriate":2,',
    '"referenceGroupHandledCorrectly":2,',
    '"interactionCoverageAdequate":2,',
    '"interactionSubstantiveCorrect":2,',
    '"uncertaintyHandlingAppropriate":2,',
    '"inferentialRegisterAppropriate":2,',
    '"mainEffectCoverageAdequate":2,',
    '"referenceGroupCoverageAdequate":2,',
    '"clarityAdequate":2,',
    '"numericExpressionAdequate":2,',
    '"comparisonStructureClear":2,',
    '"fatalFlawDetected":false,',
    '"factualScore":2,',
    '"inferenceScore":2,',
    '"completenessScore":2,',
    '"clarityScore":2,',
    '"calibrationScore":2,',
    '"overallScore":100,',
    '"overallPass":true,',
    '"llmScoringSummary":"ok",',
    '"fieldReasons":{',
    '"effectDirectionCorrect":"ok",',
    '"effectScaleAppropriate":"ok",',
    '"referenceGroupHandledCorrectly":"ok",',
    '"interactionCoverageAdequate":"ok",',
    '"interactionSubstantiveCorrect":"ok",',
    '"uncertaintyHandlingAppropriate":"ok",',
    '"inferentialRegisterAppropriate":"ok",',
    '"mainEffectCoverageAdequate":"ok",',
    '"referenceGroupCoverageAdequate":"ok",',
    '"clarityAdequate":"ok",',
    '"numericExpressionAdequate":"ok",',
    '"comparisonStructureClear":"ok",',
    '"fatalFlawDetected":"ok"',
    "}",
    "}"
  )

  calls = 0L
  chat = structure(
    list(chat = function(...) {
      calls <<- calls + 1L
      raw
    }),
    class = "ProviderFake"
  )

  runA = makeRawRunRecordForScoring()
  runA$adjustmentVariables = c("age")
  runA$primaryVariables = c("treatment")

  runB = makeRawRunRecordForScoring()
  runB$adjustmentVariables = c("sex")
  runB$primaryVariables = c("treatment")

  withr::local_options(list(warn = 2))
  cache = new.env(parent = emptyenv())
  oldCache = get0(".env_cache", inherits = TRUE, ifnotfound = NULL)
  assign(".env_cache", cache, envir = .GlobalEnv)
  withr::defer({
    if (is.null(oldCache)) {
      rm(".env_cache", envir = .GlobalEnv)
    } else {
      assign(".env_cache", oldCache, envir = .GlobalEnv)
    }
  })

  outA = scoreWmfmRunWithLlm(runA, chat = chat, useCache = TRUE)
  outB = scoreWmfmRunWithLlm(runB, chat = chat, useCache = TRUE)

  testthat::expect_identical(outA$llmScoringUsedCache, FALSE)
  testthat::expect_identical(outB$llmScoringUsedCache, FALSE)
  testthat::expect_identical(calls, 2L)
})
