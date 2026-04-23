test_that("classifyMetricDisagreement detects strict deterministic rule", {
    metricDf = data.frame(
        runId = 1:3,
        detValue = c(0, 0, 0),
        llmValue = c(2, 2, 2),
        llmMinusDet = c(2, 2, 2),
        disagrees = TRUE,
        disagreementDirection = "llmHigher"
    )

    summary = buildMetricDiagnosisSummary(metricDf, "factualScore")
    cls = classifyMetricDisagreement(summary)

    expect_equal(cls, "deterministicRuleLikelyTooStrict")
})

test_that("classifyMetricDisagreement detects no disagreement", {
    metricDf = data.frame(
        runId = 1:3,
        detValue = c(1, 2, 1),
        llmValue = c(1, 2, 1),
        llmMinusDet = 0,
        disagrees = FALSE,
        disagreementDirection = "same"
    )

    summary = buildMetricDiagnosisSummary(metricDf, "factualScore")
    cls = classifyMetricDisagreement(summary)

    expect_equal(cls, "noSystematicDisagreement")
})
