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

  callState = new.env(parent = emptyenv())
  callState$calls = 0L
  chat = structure(
    list(chat = function(...) {
      callState$calls = callState$calls + 1L
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
  rm(list = ls(envir = .env_cache), envir = .env_cache)
  withr::defer({
    rm(list = ls(envir = .env_cache), envir = .env_cache)
  })

  outA = scoreWmfmRunWithLlm(runA, chat = chat, useCache = TRUE)
  outB = scoreWmfmRunWithLlm(runB, chat = chat, useCache = TRUE)

  testthat::expect_identical(outA$llmScoringUsedCache, FALSE)
  testthat::expect_identical(outB$llmScoringUsedCache, FALSE)
  testthat::expect_identical(callState$calls, 2L)
})

testthat::test_that("scoreWmfmRunWithLlm sends follow-up scoring context to fake providers", {
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

  capturedState = new.env(parent = emptyenv())
  capturedState$prompt = NULL
  chat = structure(
    list(chat = function(prompt) {
      capturedState$prompt = prompt
      raw
    }),
    class = "ProviderFake"
  )

  runRecord = makeRawRunRecordForScoring(
    hasFollowupScoringContext = TRUE,
    followupQuestion = "Predict a future count when x is 4",
    followupCategory = "prediction_interval_request",
    followupPredictionStatus = "ok",
    followupPredictionType = "individual_prediction_interval",
    followupIntervalType = "prediction_interval",
    followupFutureObservationType = "poisson_count",
    followupExtrapolationStatus = "extrapolation_warning",
    followupExtrapolationExplanation = "WMFM computed the prediction with slight extrapolation.",
    followupParameterUncertaintyIncluded = FALSE
  )

  out = scoreWmfmRunWithLlm(runRecord, chat = chat)

  testthat::expect_identical(out$llmScored, TRUE)
  testthat::expect_match(capturedState$prompt, "FOLLOW-UP SCORING CONTEXT", fixed = TRUE)
  testthat::expect_match(capturedState$prompt, "Prediction type: individual_prediction_interval", fixed = TRUE)
  testthat::expect_match(capturedState$prompt, "Future-observation type: poisson_count", fixed = TRUE)
  testthat::expect_match(capturedState$prompt, "Use uncertaintyHandlingAppropriate", fixed = TRUE)
})

testthat::test_that("scoreWmfmRunWithLlm cache key reflects follow-up scoring context", {
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

  callState = new.env(parent = emptyenv())
  callState$calls = 0L
  chat = structure(
    list(chat = function(...) {
      callState$calls = callState$calls + 1L
      raw
    }),
    class = "ProviderFake"
  )

  runA = makeRawRunRecordForScoring(
    hasFollowupScoringContext = TRUE,
    followupPredictionStatus = "ok",
    followupPredictionType = "individual_prediction_interval",
    followupIntervalType = "prediction_interval",
    followupFutureObservationType = "poisson_count",
    followupParameterUncertaintyIncluded = FALSE
  )

  runB = makeRawRunRecordForScoring(
    hasFollowupScoringContext = TRUE,
    followupPredictionStatus = "ok",
    followupPredictionType = "future_observation",
    followupIntervalType = "none",
    followupFutureObservationType = "bernoulli",
    followupParameterUncertaintyIncluded = FALSE
  )

  withr::local_options(list(warn = 2))
  rm(list = ls(envir = .env_cache), envir = .env_cache)
  withr::defer({
    rm(list = ls(envir = .env_cache), envir = .env_cache)
  })

  outA = scoreWmfmRunWithLlm(runA, chat = chat, useCache = TRUE)
  outB = scoreWmfmRunWithLlm(runB, chat = chat, useCache = TRUE)

  testthat::expect_identical(outA$llmScoringUsedCache, FALSE)
  testthat::expect_identical(outB$llmScoringUsedCache, FALSE)
  testthat::expect_identical(callState$calls, 2L)
})
