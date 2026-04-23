test_that("summariseMetricComparison returns metricComparisonSummary", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()

    result = summariseMetricComparison(scores, comparison)

    expect_s3_class(result, "metricComparisonSummary")
    expect_s3_class(result, "data.frame")
    expect_true(all(c(
        "metric",
        "label",
        "group",
        "metricType",
        "modalProportionDeterministic",
        "entropyDeterministic",
        "nUniqueDeterministic"
    ) %in% names(result)))
})

test_that("summariseMetricComparison computes deterministic ease summaries from deterministic rows", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()

    result = summariseMetricComparison(scores, comparison)

    clarityRow = result[result$metric == "clarityScore", , drop = FALSE]

    expect_equal(nrow(clarityRow), 1)
    expect_equal(clarityRow$nUniqueDeterministic, 2)
    expect_equal(clarityRow$modalProportionDeterministic, 2 / 3)
    expect_true(is.finite(clarityRow$entropyDeterministic))
})

test_that("summariseMetricComparison merges disagreement summaries by metric", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()

    result = summariseMetricComparison(scores, comparison)

    factualRow = result[result$metric == "factualScore", , drop = FALSE]
    overallRow = result[result$metric == "overallScore", , drop = FALSE]

    expect_equal(factualRow$meanAbsoluteDifference, 2 / 3)
    expect_equal(factualRow$proportionEqual, 1 / 3)
    expect_equal(factualRow$proportionAdjacent, 2 / 3)
    expect_equal(factualRow$weightedKappa, 0.4)

    expect_equal(overallRow$meanAbsoluteDifference, 2 / 3)
    expect_equal(overallRow$correlation, 0.9)
})

test_that("summariseMetricComparison can order by disagreement", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()

    result = summariseMetricComparison(
        scores = scores,
        comparison = comparison,
        orderBy = "disagreement"
    )

    expect_equal(result$metric[1], "factualScore")
})

test_that("summariseMetricComparison can order by ease", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()

    result = summariseMetricComparison(
        scores = scores,
        comparison = comparison,
        orderBy = "ease"
    )

    expect_equal(result$metric, c("clarityScore", "factualScore", "overallScore"))
})

test_that("summariseMetricComparison validates inputs and options", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()

    expect_error(
        summariseMetricComparison(mtcars, comparison),
        "scores",
        ignore.case = TRUE
    )

    expect_error(
        summariseMetricComparison(scores, mtcars),
        "comparison",
        ignore.case = TRUE
    )

    expect_error(
        summariseMetricComparison(scores, comparison, orderBy = "somethingElse"),
        "orderBy",
        ignore.case = TRUE
    )
})

test_that("summariseMetricComparison errors when registry is missing", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()
    comparison$registry = data.frame()

    expect_error(
        summariseMetricComparison(scores, comparison),
        "registry",
        ignore.case = TRUE
    )
})

test_that("summariseMetricComparison errors when deterministic method has no rows", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()

    expect_error(
        summariseMetricComparison(
            scores = scores,
            comparison = comparison,
            deterministicMethod = "rubric2"
        ),
        "No rows found for deterministic method",
        ignore.case = TRUE
    )
})

test_that("summariseMetricComparison errors when no deterministic metric summaries can be derived", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()
    comparison$registry$metricName = c("metricA", "metricB", "metricC")

    expect_error(
        summariseMetricComparison(scores, comparison),
        "Could not derive deterministic metric summaries",
        ignore.case = TRUE
    )
})

test_that("summariseMetricComparison retains registry metrics when agreement summaries are absent", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeExampleMetricComparison()
    comparison$binaryAgreement = data.frame()
    comparison$ordinalAgreement = data.frame()
    comparison$continuousAgreement = data.frame()

    result = summariseMetricComparison(scores, comparison)

    expect_equal(result$metric, c("factualScore", "clarityScore", "overallScore"))
    expect_true(all(is.na(result$meanAbsoluteDifference)))
})
