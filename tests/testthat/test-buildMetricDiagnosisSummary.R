test_that("buildMetricDiagnosisSummary computes expected summary statistics", {
  metricDf = data.frame(
    runId = 1:4,
    detValue = c(0, 0, 0, 0),
    llmValue = c(2, 2, 2, 2),
    llmMinusDet = c(2, 2, 2, 2),
    disagrees = c(TRUE, TRUE, TRUE, TRUE),
    disagreementDirection = c("llmHigher", "llmHigher", "llmHigher", "llmHigher"),
    stringsAsFactors = FALSE
  )

  out = buildMetricDiagnosisSummary(
    metricDf = metricDf,
    metric = "numericExpressionAdequate"
  )

  expect_equal(out$nRuns, 4)
  expect_equal(out$nDisagree, 4)
  expect_equal(out$disagreementRate, 1)
  expect_equal(out$meanDet, 0)
  expect_equal(out$meanLlm, 2)
  expect_equal(out$meanLlmMinusDet, 2)
  expect_equal(out$directionConsistency, 1)
  expect_true(out$detConstant)
  expect_true(out$llmConstant)
})
