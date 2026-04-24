test_that("selectFlaggedMetricRuns returns sorted disagreements", {
    metricDf = data.frame(
        runId = 1:4,
        llmMinusDet = c(0, 2, -3, 1),
        disagrees = c(FALSE, TRUE, TRUE, TRUE)
    )

    result = selectFlaggedMetricRuns(metricDf, maxExamples = 2)

    expect_equal(nrow(result), 2)
    expect_equal(result$runId, c(3, 2))
})
