test_that("getMetricComparisonData returns metricComparisonData with llm comparison columns", {
    comparison = makeExampleMetricComparison()

    result = getMetricComparisonData(comparison, metric = "factualScore")

    expect_s3_class(result, "metricComparisonData")
    expect_s3_class(result, "data.frame")
    expect_equal(attr(result, "metric"), "factualScore")
    expect_equal(attr(result, "leftMethod"), "deterministic")
    expect_equal(attr(result, "rightMethod"), "llm")

    expect_true(all(c(
        "runId",
        "detValue",
        "llmValue",
        "llmMinusDet",
        "detMinusLlm",
        "absDifference",
        "agreementClass"
    ) %in% names(result)))

    expect_equal(result$detValue, c(2, 1, 2))
    expect_equal(result$llmValue, c(2, 2, 1))
    expect_equal(result$llmMinusDet, c(0, 1, -1))
    expect_equal(result$detMinusLlm, c(0, -1, 1))
    expect_equal(result$absDifference, c(0, 1, 1))
    expect_equal(result$agreementClass, c("exact", "adjacent", "adjacent"))
})

test_that("getMetricComparisonData uses llm-specific columns when the other method is llm", {
    comparison = makeExampleMetricComparison()

    result = getMetricComparisonData(comparison, metric = "clarityScore")

    expect_equal(attr(result, "rightMethod"), "llm")
    expect_true("llmValue" %in% names(result))
    expect_true("llmMinusDet" %in% names(result))
    expect_false("difference" %in% names(result))
    expect_equal(result$llmMinusDet, c(0, 1, 0))
})

test_that("getMetricComparisonData uses generic difference for non-llm methods", {
    comparison = makeExampleMetricComparison(
        leftMethod = "deterministic",
        rightMethod = "rubric2"
    )

    result = getMetricComparisonData(comparison, metric = "clarityScore")

    expect_equal(attr(result, "rightMethod"), "rubric2")
    expect_true("rubric2Value" %in% names(result))
    expect_true("difference" %in% names(result))
    expect_false("llmMinusDet" %in% names(result))
    expect_equal(result$difference, c(0, 1, 0))
})

test_that("getMetricComparisonData requires metric to be supplied", {
    comparison = makeExampleMetricComparison()

    expect_error(
        getMetricComparisonData(comparison),
        "metric.*must be supplied",
        ignore.case = TRUE
    )
})

test_that("getMetricComparisonData rejects empty pairData", {
    comparison = makeExampleMetricComparison()
    comparison$pairData = comparison$pairData[0, , drop = FALSE]

    expect_error(
        getMetricComparisonData(comparison, metric = "factualScore"),
        "pairData.*empty",
        ignore.case = TRUE
    )
})

test_that("getMetricComparisonData validates metric against available choices", {
    comparison = makeExampleMetricComparison()

    expect_error(
        getMetricComparisonData(comparison, metric = "notARealMetric"),
        "should be one of",
        ignore.case = TRUE
    )
})
