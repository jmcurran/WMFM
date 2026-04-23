testthat::test_that("parseWmfmScoringJson parses fenced JSON", {
  raw = paste(
    "```json",
    '{"effectDirectionCorrect":2,"effectScaleAppropriate":2,"referenceGroupHandledCorrectly":2,"interactionCoverageAdequate":2,"interactionSubstantiveCorrect":2,"uncertaintyHandlingAppropriate":2,"inferentialRegisterAppropriate":2,"mainEffectCoverageAdequate":2,"referenceGroupCoverageAdequate":2,"clarityAdequate":2,"numericExpressionAdequate":2,"comparisonStructureClear":2,"fatalFlawDetected":false,"factualScore":2,"inferenceScore":2,"completenessScore":2,"clarityScore":2,"calibrationScore":2,"overallScore":2,"overallPass":true,"llmScoringSummary":"Good","fieldReasons":{"effectDirectionCorrect":"ok","effectScaleAppropriate":"ok","referenceGroupHandledCorrectly":"ok","interactionCoverageAdequate":"ok","interactionSubstantiveCorrect":"ok","uncertaintyHandlingAppropriate":"ok","inferentialRegisterAppropriate":"ok","mainEffectCoverageAdequate":"ok","referenceGroupCoverageAdequate":"ok","clarityAdequate":"ok","numericExpressionAdequate":"ok","comparisonStructureClear":"ok","fatalFlawDetected":"ok"}}',
    "```",
    sep = "\n"
  )

  parsed = parseWmfmScoringJson(raw)

  testthat::expect_true(is.list(parsed))
  testthat::expect_equal(parsed$effectDirectionCorrect, 2)
  testthat::expect_equal(parsed$overallPass, FALSE | TRUE)
})

testthat::test_that("parseWmfmScoringJson rejects missing JSON", {
  testthat::expect_error(
    parseWmfmScoringJson("not json"),
    "JSON object"
  )
})
