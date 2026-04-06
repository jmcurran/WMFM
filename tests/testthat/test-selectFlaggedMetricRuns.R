test_that("selectFlaggedMetricRuns returns only disagreement rows", {
  metricDf = data.frame(
    runId = 1:4,
    llmMinusDet = c(2, 0, -1, 3),
    disagrees = c(TRUE, FALSE, TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  out = selectFlaggedMetricRuns(metricDf, maxExamples = 2)

  expect_equal(nrow(out), 2)
  expect_true(all(out$disagrees))
})

test_that("selectFlaggedMetricRuns orders by absolute difference", {
  metricDf = data.frame(
    runId = 1:4,
    llmMinusDet = c(1, -3, 2, 0),
    disagrees = c(TRUE, TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  out = selectFlaggedMetricRuns(metricDf, maxExamples = 3)

  expect_equal(out$runId, c(2, 3, 1))
})
