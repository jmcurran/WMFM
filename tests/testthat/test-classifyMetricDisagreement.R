test_that("classifyMetricDisagreement identifies no disagreement", {
  summaryDf = data.frame(
    disagreementRate = 0,
    directionConsistency = 0,
    detConstant = TRUE,
    llmConstant = TRUE,
    detMin = 1,
    detMax = 1,
    llmMin = 1,
    llmMax = 1,
    meanLlmMinusDet = 0
  )

  expect_equal(
    classifyMetricDisagreement(summaryDf),
    "noSystematicDisagreement"
  )
})

test_that("classifyMetricDisagreement identifies deterministicRuleLikelyTooStrict", {
  summaryDf = data.frame(
    disagreementRate = 1,
    directionConsistency = 1,
    detConstant = TRUE,
    llmConstant = TRUE,
    detMin = 0,
    detMax = 0,
    llmMin = 2,
    llmMax = 2,
    meanLlmMinusDet = 2
  )

  expect_equal(
    classifyMetricDisagreement(summaryDf),
    "deterministicRuleLikelyTooStrict"
  )
})

test_that("classifyMetricDisagreement identifies rubric ambiguity pattern", {
  summaryDf = data.frame(
    disagreementRate = 0.7,
    directionConsistency = 0.5,
    detConstant = FALSE,
    llmConstant = FALSE,
    detMin = 0,
    detMax = 2,
    llmMin = 0,
    llmMax = 2,
    meanLlmMinusDet = 0.2
  )

  expect_equal(
    classifyMetricDisagreement(summaryDf),
    "rubricOrMetricMayBeAmbiguous"
  )
})
