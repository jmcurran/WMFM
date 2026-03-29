makeRunRecordForScoring = function() {
  list(
    runId = 1L,
    exampleName = "Course",
    package = "WMFM",
    modelType = "lm",
    formula = "Exam ~ Attend + Test",
    equationsText = "Exam = 6.62 + 3.52 * Test",
    explanationText = "Attendance is associated with higher marks.",
    interactionTerms = "",
    hasInteractionTerms = FALSE,
    nInteractionTerms = 0L,
    interactionMinPValue = NA_real_,
    interactionAlpha = 0.05,
    hasError = FALSE,
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

makeFakeChat = function(rawResponse) {
  structure(
    list(
      chat = function(prompt) {
        rawResponse
      }
    ),
    class = "ProviderFake"
  )
}

testthat::test_that("scoreWmfmRunWithLlm scores one run", {
  raw = '{"effectDirectionCorrect":2,"effectScaleAppropriate":2,"referenceGroupHandledCorrectly":1,"interactionCoverageAdequate":2,"interactionSubstantiveCorrect":2,"uncertaintyHandlingAppropriate":1,"inferentialRegisterAppropriate":1,"mainEffectCoverageAdequate":2,"referenceGroupCoverageAdequate":1,"clarityAdequate":2,"numericExpressionAdequate":2,"comparisonStructureClear":2,"fatalFlawDetected":false,"factualScore":1.8,"inferenceScore":1.4,"completenessScore":1.7,"clarityScore":1.9,"calibrationScore":1.3,"overallScore":1.6,"overallPass":true,"llmScoringSummary":"Mostly correct.","fieldReasons":{"effectDirectionCorrect":"ok","effectScaleAppropriate":"ok","referenceGroupHandledCorrectly":"ok","interactionCoverageAdequate":"ok","interactionSubstantiveCorrect":"ok","uncertaintyHandlingAppropriate":"ok","inferentialRegisterAppropriate":"ok","mainEffectCoverageAdequate":"ok","referenceGroupCoverageAdequate":"ok","clarityAdequate":"ok","numericExpressionAdequate":"ok","comparisonStructureClear":"ok","fatalFlawDetected":"ok"}}'

  chat = makeFakeChat(raw)
  out = scoreWmfmRunWithLlm(makeRunRecordForScoring(), chat = chat)

  testthat::expect_identical(out$llmScored, TRUE)
  testthat::expect_identical(out$effectDirectionCorrect, 2L)
})

testthat::test_that("scoreWmfmRunWithLlm skips errored runs", {
  x = makeRunRecordForScoring()
  x$hasError = TRUE

  out = scoreWmfmRunWithLlm(x, chat = makeFakeChat("{}"))

  testthat::expect_identical(out$llmScored, FALSE)
})
