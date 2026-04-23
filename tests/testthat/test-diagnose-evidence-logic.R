test_that("buildMetricEvidenceSummary flags missing effect scale extraction", {
    metricDf = data.frame(
        runId = 1:5,
        disagrees = c(TRUE, TRUE, TRUE, TRUE, TRUE),
        effectScaleClaim = c("not_stated", "not_stated", "not_stated", "additive", "not_stated"),
        percentLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        comparisonLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        conditionalLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )

    out = buildMetricEvidenceSummary(
        metricDf = metricDf,
        metric = "numericExpressionAdequate"
    )

    expect_equal(out$effectScale_notStated_n, 4)
    expect_equal(out$likelyIssue, "effectScaleExtractionOftenMissing")
})

test_that("buildMetricEvidenceSummary flags unclear effect scale extraction", {
    metricDf = data.frame(
        runId = 1:5,
        disagrees = c(TRUE, TRUE, TRUE, TRUE, TRUE),
        effectScaleClaim = c(
            "mixed_or_unclear",
            "mixed_or_unclear",
            "mixed_or_unclear",
            "mixed_or_unclear",
            "additive"
        ),
        percentLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        comparisonLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        conditionalLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )

    out = buildMetricEvidenceSummary(
        metricDf = metricDf,
        metric = "numericExpressionAdequate"
    )

    expect_equal(out$effectScale_mixedOrUnclear_n, 4)
    expect_equal(out$likelyIssue, "effectScaleExtractionOftenUnclear")
})

test_that("buildMetricEvidenceSummary flags additive numeric language without percent wording", {
    metricDf = data.frame(
        runId = 1:5,
        disagrees = c(TRUE, TRUE, TRUE, TRUE, TRUE),
        effectScaleClaim = c("additive", "additive", "additive", "not_stated", "not_stated"),
        percentLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        comparisonLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        conditionalLanguageMention = c(FALSE, FALSE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )

    out = buildMetricEvidenceSummary(
        metricDf = metricDf,
        metric = "numericExpressionAdequate"
    )

    expect_equal(out$effectScale_additive_n, 3)
    expect_equal(out$percentLanguageMention_rate, 0)
    expect_equal(out$likelyIssue, "ruleMayUndervalueAdditiveNumericLanguage")
})

test_that("buildMetricEvidenceSummary flags missed numeric content when comparison or conditional wording is common", {
  metricDf = data.frame(
    runId = 1:4,
    disagrees = c(TRUE, TRUE, TRUE, TRUE),
    effectScaleClaim = c(
      "additive",
      "multiplicative",
      "multiplicative",
      "multiplicative"
    ),
    percentLanguageMention = c(FALSE, FALSE, FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, TRUE, TRUE, FALSE),
    conditionalLanguageMention = c(FALSE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  out = buildMetricEvidenceSummary(
    metricDf = metricDf,
    metric = "numericExpressionAdequate"
  )

  expect_equal(out$effectScale_additive_n, 1)
  expect_equal(out$comparisonLanguageMention_rate, 0.75)
  expect_equal(out$likelyIssue, "numericContentPresentButRuleInputsMayMissIt")
})
test_that("buildMetricEvidenceSummary returns insufficientContext when disagreement exists but no stronger heuristic fires", {
    metricDf = data.frame(
        runId = 1:4,
        disagrees = c(TRUE, TRUE, FALSE, FALSE),
        effectScaleClaim = c("additive", "multiplicative", "additive", "multiplicative"),
        percentLanguageMention = c(TRUE, TRUE, TRUE, TRUE),
        comparisonLanguageMention = c(FALSE, FALSE, FALSE, FALSE),
        conditionalLanguageMention = c(FALSE, FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )

    out = buildMetricEvidenceSummary(
        metricDf = metricDf,
        metric = "numericExpressionAdequate"
    )

    expect_equal(out$nDisagree, 2)
    expect_equal(out$likelyIssue, "insufficientContext")
})

test_that("buildMetricEvidenceSummary returns no likely issue when there is no disagreement", {
    metricDf = data.frame(
        runId = 1:3,
        disagrees = c(FALSE, FALSE, FALSE),
        effectScaleClaim = c("not_stated", "not_stated", "additive"),
        percentLanguageMention = c(FALSE, FALSE, FALSE),
        comparisonLanguageMention = c(TRUE, TRUE, TRUE),
        conditionalLanguageMention = c(FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )

    out = buildMetricEvidenceSummary(
        metricDf = metricDf,
        metric = "numericExpressionAdequate"
    )

    expect_true(is.na(out$likelyIssue))
})

test_that("buildMetricEvidenceSummary uses generic fallback for non-numeric-expression metrics", {
    metricDf = data.frame(
        runId = 1:3,
        disagrees = c(TRUE, TRUE, FALSE),
        stringsAsFactors = FALSE
    )

    out = buildMetricEvidenceSummary(
        metricDf = metricDf,
        metric = "clarityAdequate"
    )

    expect_equal(out$likelyIssue, "metricSpecificEvidenceNotYetImplemented")
})

test_that("diagnose merges runs context into evidence summary for numericExpressionAdequate", {
    scores = structure(
        list(
            scores = list(
                deterministic = list(
                    list(numericExpressionAdequate = 0),
                    list(numericExpressionAdequate = 0),
                    list(numericExpressionAdequate = 0)
                ),
                llm = list(
                    list(numericExpressionAdequate = 2),
                    list(numericExpressionAdequate = 2),
                    list(numericExpressionAdequate = 2)
                )
            )
        ),
        class = "wmfmScores"
    )

    runs = data.frame(
        runId = 1:3,
        explanationText = c(
            "about two points higher",
            "increase by 2",
            "roughly two units higher"
        ),
        effectScaleClaim = c("not_stated", "not_stated", "additive"),
        percentLanguageMention = c(FALSE, FALSE, FALSE),
        comparisonLanguageMention = c(TRUE, TRUE, TRUE),
        conditionalLanguageMention = c(FALSE, FALSE, FALSE),
        stringsAsFactors = FALSE
    )

    dx = diagnose(
        scores,
        metric = "numericExpressionAdequate",
        runs = runs
    )

    expect_equal(dx$evidenceSummary$effectScale_notStated_n, 2)
    expect_equal(dx$evidenceSummary$effectScale_additive_n, 1)
    expect_equal(
        dx$evidenceSummary$likelyIssue,
        "effectScaleExtractionOftenMissing"
    )
})
