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
