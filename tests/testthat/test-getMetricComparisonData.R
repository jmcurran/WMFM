test_that("getMetricComparisonData returns metricComparisonData with llm comparison columns", {
    comparison = makeMetricComparisonObject()

    result = getMetricComparisonData(comparison, metric = "factualScore")

    expect_s3_class(result, "metricComparisonData")
    expect_equal(attr(result, "metric"), "factualScore")
    expect_equal(attr(result, "leftMethod"), "deterministic")
    expect_equal(attr(result, "rightMethod"), "llm")
    expect_named(
        result,
        c(
            "runId",
            "detValue",
            "llmValue",
            "llmMinusDet",
            "detMinusLlm",
            "absDifference",
            "agreementClass"
        )
    )
    expect_equal(result$detValue, c(2, 1, 0))
    expect_equal(result$llmValue, c(2, 2, 1))
    expect_equal(result$llmMinusDet, c(0, 1, 1))
    expect_equal(result$detMinusLlm, c(0, -1, -1))
    expect_equal(result$absDifference, c(0, 1, 1))
    expect_equal(result$agreementClass, c("exact", "adjacent", "adjacent"))
})

test_that("getMetricComparisonData uses non-deterministic method name in output column", {
    comparison = makeMetricComparisonObjectWithoutDeterministic()

    result = getMetricComparisonData(comparison, metric = "clarityScore")

    expect_true("llmValue" %in% names(result))
    expect_true("difference" %in% names(result))
    expect_false("llmMinusDet" %in% names(result))
    expect_equal(result$difference, c(0, 1, 0))
})

test_that("getMetricComparisonData requires a wmfmScoreComparison object", {
    expect_error(
        getMetricComparisonData(mtcars, metric = "mpg"),
        "wmfmScoreComparison"
    )
})

test_that("getMetricComparisonData requires metric to be supplied", {
    comparison = makeMetricComparisonObject()

    expect_error(
        getMetricComparisonData(comparison),
        "metric.*must be supplied"
    )
})

test_that("getMetricComparisonData rejects empty pairData", {
    comparison = makeMetricComparisonObjectWithEmptyPairData()

    expect_error(
        getMetricComparisonData(comparison, metric = "factualScore"),
        "pairData.*empty"
    )
})

test_that("getMetricComparisonData validates metric against available choices", {
    comparison = makeMetricComparisonObject()

    expect_error(
        getMetricComparisonData(comparison, metric = "notARealMetric"),
        "arg should be one of"
    )
})
