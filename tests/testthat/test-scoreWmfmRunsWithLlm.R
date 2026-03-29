testthat::test_that("scoreWmfmRunsWithLlm handles a list of runs", {
  raw = '{"effectDirectionCorrect":2,"effectScaleAppropriate":2,"referenceGroupHandledCorrectly":1,"interactionCoverageAdequate":2,"interactionSubstantiveCorrect":2,"uncertaintyHandlingAppropriate":1,"inferentialRegisterAppropriate":1,"mainEffectCoverageAdequate":2,"referenceGroupCoverageAdequate":1,"clarityAdequate":2,"numericExpressionAdequate":2,"comparisonStructureClear":2,"fatalFlawDetected":false,"factualScore":1.8,"inferenceScore":1.4,"completenessScore":1.7,"clarityScore":1.9,"calibrationScore":1.3,"overallScore":1.6,"overallPass":true,"llmScoringSummary":"Mostly correct.","fieldReasons":{"effectDirectionCorrect":"ok","effectScaleAppropriate":"ok","referenceGroupHandledCorrectly":"ok","interactionCoverageAdequate":"ok","interactionSubstantiveCorrect":"ok","uncertaintyHandlingAppropriate":"ok","inferentialRegisterAppropriate":"ok","mainEffectCoverageAdequate":"ok","referenceGroupCoverageAdequate":"ok","clarityAdequate":"ok","numericExpressionAdequate":"ok","comparisonStructureClear":"ok","fatalFlawDetected":"ok"}}'

  chat = makeFakeChat(raw)
  runs = list(
    a = makeRunRecordForScoring(),
    b = makeRunRecordForScoring()
  )

  out = scoreWmfmRunsWithLlm(runs, chat = chat, showProgress = FALSE)

  testthat::expect_length(out, 2)
  testthat::expect_identical(out[[1]]$llmScored, TRUE)
  testthat::expect_identical(out[[2]]$effectScaleAppropriate, 2L)
})

testthat::test_that("scoreWmfmRunsWithLlm dispatches single run records", {
  raw = '{"effectDirectionCorrect":2,"effectScaleAppropriate":2,"referenceGroupHandledCorrectly":1,"interactionCoverageAdequate":2,"interactionSubstantiveCorrect":2,"uncertaintyHandlingAppropriate":1,"inferentialRegisterAppropriate":1,"mainEffectCoverageAdequate":2,"referenceGroupCoverageAdequate":1,"clarityAdequate":2,"numericExpressionAdequate":2,"comparisonStructureClear":2,"fatalFlawDetected":false,"factualScore":1.8,"inferenceScore":1.4,"completenessScore":1.7,"clarityScore":1.9,"calibrationScore":1.3,"overallScore":1.6,"overallPass":true,"llmScoringSummary":"Mostly correct.","fieldReasons":{"effectDirectionCorrect":"ok","effectScaleAppropriate":"ok","referenceGroupHandledCorrectly":"ok","interactionCoverageAdequate":"ok","interactionSubstantiveCorrect":"ok","uncertaintyHandlingAppropriate":"ok","inferentialRegisterAppropriate":"ok","mainEffectCoverageAdequate":"ok","referenceGroupCoverageAdequate":"ok","clarityAdequate":"ok","numericExpressionAdequate":"ok","comparisonStructureClear":"ok","fatalFlawDetected":"ok"}}'

  chat = makeFakeChat(raw)
  out = scoreWmfmRunsWithLlm(makeRunRecordForScoring(), chat = chat, showProgress = FALSE)

  testthat::expect_identical(out$llmScored, TRUE)
})
