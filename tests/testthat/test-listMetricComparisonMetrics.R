test_that("listMetricComparisonMetrics returns shared numeric metrics", {
    scores = makeMetricComparisonScoresFixture()

    result = listMetricComparisonMetrics(scores)

    expect_type(result, "character")
    expect_equal(result, c("clarityScore", "factualScore", "overallScore"))
})

test_that("listMetricComparisonMetrics excludes known non-comparison fields", {
    scores = makeMetricComparisonScoresFixture()

    result = listMetricComparisonMetrics(scores)

    expect_false("llmScored" %in% result)
    expect_false("llmScoringModel" %in% result)
    expect_false("primaryScoringMethod" %in% result)
    expect_false("interactionEvidenceAppropriate" %in% result)
})

test_that("listMetricComparisonMetrics requires a wmfmScores object", {
    expect_error(
        listMetricComparisonMetrics(mtcars),
        "wmfmScores object"
    )
})

test_that("listMetricComparisonMetrics requires both deterministic and llm scores", {
    scores = makeMetricComparisonScoresWithoutLlm()

    expect_error(
        listMetricComparisonMetrics(scores),
        "Both deterministic and llm scores must be present"
    )
})

test_that("listMetricComparisonMetrics drops metrics without usable numeric values on both sides", {
    scores = makeMetricComparisonScoresFixture()
    scores$scores$deterministic = lapply(scores$scores$deterministic, function(run) {
        run$nonNumericMetric = "low"
        run
    })
    scores$scores$llm = lapply(scores$scores$llm, function(run) {
        run$nonNumericMetric = "high"
        run
    })
    scores$scores$deterministic[[1]]$detOnlyMetric = 1
    scores$scores$llm[[1]]$detOnlyMetric = NA_real_

    result = listMetricComparisonMetrics(scores)

    expect_false("nonNumericMetric" %in% result)
    expect_false("detOnlyMetric" %in% result)
})
