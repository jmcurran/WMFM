test_that("summariseMetricComparison returns metricComparisonSummary", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()

    longDf = as.data.frame(scores, format = "long")
    expect_equal(nrow(longDf), 6)

    result = summariseMetricComparison(scores, comparison)

    expect_s3_class(result, "metricComparisonSummary")
    expect_named(
        result,
        c(
            "metric",
            "label",
            "group",
            "metricType",
            "modalProportionDeterministic",
            "entropyDeterministic",
            "nUniqueDeterministic",
            "meanAbsoluteDifference",
            "proportionEqual",
            "proportionAdjacent",
            "weightedKappa",
            "correlation"
        )
    )
    expect_equal(result$metric, c("factualScore", "clarityScore", "overallScore"))
})

test_that("summariseMetricComparison computes deterministic ease summaries from deterministic rows", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()

    result = summariseMetricComparison(scores, comparison)

    factualRow = result[result$metric == "factualScore", , drop = FALSE]
    clarityRow = result[result$metric == "clarityScore", , drop = FALSE]
    overallRow = result[result$metric == "overallScore", , drop = FALSE]

    expect_equal(factualRow$modalProportionDeterministic, 1 / 3)
    expect_equal(factualRow$nUniqueDeterministic, 3)
    expect_equal(clarityRow$modalProportionDeterministic, 2 / 3)
    expect_equal(clarityRow$nUniqueDeterministic, 2)
    expect_equal(overallRow$nUniqueDeterministic, 3)
})

test_that("summariseMetricComparison merges disagreement summaries by metric", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()

    result = summariseMetricComparison(scores, comparison)

    factualRow = result[result$metric == "factualScore", , drop = FALSE]
    clarityRow = result[result$metric == "clarityScore", , drop = FALSE]
    overallRow = result[result$metric == "overallScore", , drop = FALSE]

    expect_equal(factualRow$meanAbsoluteDifference, 2 / 3)
    expect_equal(factualRow$proportionEqual, 1 / 3)
    expect_equal(factualRow$proportionAdjacent, 2 / 3)
    expect_equal(factualRow$weightedKappa, 0.25)

    expect_equal(clarityRow$meanAbsoluteDifference, 1 / 3)
    expect_equal(clarityRow$proportionEqual, 2 / 3)
    expect_equal(clarityRow$proportionAdjacent, 1 / 3)
    expect_equal(clarityRow$weightedKappa, 0.50)

    expect_equal(overallRow$meanAbsoluteDifference, 10 / 3)
    expect_true(is.na(overallRow$proportionEqual))
    expect_equal(overallRow$correlation, 1)
})

test_that("summariseMetricComparison can order by disagreement", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()

    result = summariseMetricComparison(scores, comparison, orderBy = "disagreement")

    expect_equal(result$metric, c("overallScore", "factualScore", "clarityScore"))
})

test_that("summariseMetricComparison can order by ease", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()

    result = summariseMetricComparison(scores, comparison, orderBy = "ease")

    expect_equal(result$metric, c("factualScore", "overallScore", "clarityScore"))
})

test_that("summariseMetricComparison validates inputs and options", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()

    expect_error(
        summariseMetricComparison(mtcars, comparison),
        "scores.*wmfmScores"
    )

    expect_error(
        summariseMetricComparison(scores, mtcars),
        "comparison.*wmfmScoreComparison"
    )

    expect_error(
        summariseMetricComparison(scores, comparison, orderBy = "size"),
        "orderBy.*NULL, 'disagreement', or 'ease'"
    )
})

test_that("summariseMetricComparison errors when registry is missing", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()
    comparison$registry = data.frame()

    expect_error(
        summariseMetricComparison(scores, comparison),
        "registry.*non-empty data.frame"
    )
})

test_that("summariseMetricComparison errors when deterministic method has no rows", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()

    expect_error(
        summariseMetricComparison(
            scores,
            comparison,
            deterministicMethod = "ruleBased"
        ),
        "No rows found for deterministic method"
    )
})

test_that("summariseMetricComparison errors when no deterministic metric summaries can be derived", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObject()
    comparison$registry$metricName = c("missingOne", "missingTwo", "missingThree")

    expect_error(
        summariseMetricComparison(scores, comparison),
        "Could not derive deterministic metric summaries"
    )
})

test_that("summariseMetricComparison retains registry metrics when agreement summaries are absent", {
    scores = makeMetricComparisonScoresFixture()
    comparison = makeMetricComparisonObjectForSummaryWithMissingAgreement()

    result = summariseMetricComparison(scores, comparison)

    expect_equal(result$metric, c("factualScore", "clarityScore", "overallScore"))
    expect_true(all(is.na(result$meanAbsoluteDifference)))
    expect_true(all(is.na(result$proportionEqual)))
    expect_true(all(is.na(result$weightedKappa)))
    expect_true(all(is.na(result$correlation)))
})
