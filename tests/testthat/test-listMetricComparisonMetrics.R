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
    expect_false("interactionEvidenceAppropriate" %in% result)
})

test_that("listMetricComparisonMetrics requires a wmfmScores object", {
    expect_error(
        listMetricComparisonMetrics(mtcars),
        "wmfmScores",
        ignore.case = TRUE
    )
})

test_that("listMetricComparisonMetrics requires both deterministic and llm scores", {
    scores = makeMetricComparisonScoresWithoutLlm()

    expect_error(
        listMetricComparisonMetrics(scores),
        "Both deterministic and llm scores must be present",
        ignore.case = TRUE
    )
})

test_that("listMetricComparisonMetrics drops metrics without usable numeric values on both sides", {
    scores = makeMetricComparisonScoresFixture()
    scores$scores$deterministic[[1]]$textOnly = "alpha"
    scores$scores$deterministic[[2]]$textOnly = "beta"
    scores$scores$deterministic[[3]]$textOnly = "gamma"
    scores$scores$llm[[1]]$textOnly = "delta"
    scores$scores$llm[[2]]$textOnly = "epsilon"
    scores$scores$llm[[3]]$textOnly = "zeta"

    result = listMetricComparisonMetrics(scores)

    expect_false("textOnly" %in% result)
})
