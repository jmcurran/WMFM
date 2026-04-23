test_that("buildMetricEvidenceSummary returns metric-aware summary for numericExpressionAdequate", {
  metricDf = data.frame(
    runId = 1:4,
    detValue = c(0, 0, 0, 0),
    llmValue = c(2, 2, 2, 2),
    disagrees = c(TRUE, TRUE, TRUE, TRUE),
    effectScaleClaim = c("not_stated", "not_stated", "additive", "not_stated"),
    percentLanguageMention = c(FALSE, FALSE, FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, TRUE, FALSE, TRUE),
    conditionalLanguageMention = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  out = buildMetricEvidenceSummary(
    metricDf = metricDf,
    metric = "numericExpressionAdequate"
  )

  expect_true(is.data.frame(out))
  expect_equal(out$metric, "numericExpressionAdequate")
  expect_equal(out$effectScale_notStated_n, 3)
  expect_equal(out$effectScale_additive_n, 1)
  expect_equal(out$percentLanguageMention_rate, 0)
  expect_equal(out$likelyIssue, "effectScaleExtractionOftenMissing")
})

test_that("buildMetricEvidenceSummary returns generic summary for other metrics", {
  metricDf = data.frame(
    runId = 1:3,
    detValue = c(1, 1, 1),
    llmValue = c(1, 2, 1),
    disagrees = c(FALSE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  out = buildMetricEvidenceSummary(
    metricDf = metricDf,
    metric = "clarityAdequate"
  )

  expect_true(is.data.frame(out))
  expect_equal(out$metric, "clarityAdequate")
  expect_equal(out$likelyIssue, "metricSpecificEvidenceNotYetImplemented")
})
