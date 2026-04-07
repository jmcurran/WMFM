test_that("stability dispatches to the wmfmScores method", {
    scores = makeExampleWmfmScoresForStability()

    result = stability(scores)

    expect_s3_class(result, "wmfmScoreStability")
    expect_equal(result$methods, c("deterministic", "llm"))
    expect_equal(result$nRuns, 3)
})

test_that("stability errors cleanly for an unsupported class", {
    expect_error(
        stability(mtcars),
        regexp = "no applicable method"
    )
})

test_that("stability.wmfmScores errors when no long scoring rows are available", {
    scores = makeEmptyLongWmfmScores()

    expect_error(
        stability(scores),
        regexp = "No scoring rows are available"
    )
})

test_that("stability summarises binary fields by method", {
    scores = makeExampleWmfmScoresForStability()

    result = stability(scores)
    binary = result$binaryStability

    expect_s3_class(binary, "data.frame")
    expect_true(all(c(
        "method",
        "metric",
        "n",
        "trueRate",
        "falseRate",
        "modalValue",
        "modalProportion",
        "variesAcrossRuns"
    ) %in% names(binary)))

    fatalDet = binary[
        binary$method == "deterministic" &
            binary$metric == "fatalFlawDetected",
        ,
        drop = FALSE
    ]

    expect_equal(nrow(fatalDet), 1)
    expect_equal(fatalDet$n, 3)
    expect_equal(fatalDet$trueRate, 1 / 3)
    expect_equal(fatalDet$falseRate, 2 / 3)
    expect_identical(fatalDet$variesAcrossRuns, TRUE)
})

test_that("stability summarises ordinal fields by method", {
    scores = makeExampleWmfmScoresForStability()

    result = stability(scores)
    ordinal = result$ordinalStability

    expect_s3_class(ordinal, "data.frame")
    expect_true(all(c(
        "method",
        "metric",
        "n",
        "mean",
        "sd",
        "min",
        "max",
        "range",
        "modalValue",
        "modalProportion",
        "variesAcrossRuns"
    ) %in% names(ordinal)))

    effectDet = ordinal[
        ordinal$method == "deterministic" &
            ordinal$metric == "effectDirectionCorrect",
        ,
        drop = FALSE
    ]

    expect_equal(nrow(effectDet), 1)
    expect_equal(effectDet$n, 3)
    expect_equal(effectDet$min, 1)
    expect_equal(effectDet$max, 2)
    expect_equal(effectDet$range, 1)
    expect_identical(effectDet$variesAcrossRuns, TRUE)
})

test_that("stability summarises continuous fields by method", {
    scores = makeExampleWmfmScoresForStability()

    result = stability(scores)
    continuous = result$continuousStability

    expect_s3_class(continuous, "data.frame")
    expect_true(all(c(
        "method",
        "metric",
        "n",
        "mean",
        "sd",
        "min",
        "max",
        "range"
    ) %in% names(continuous)))

    overallLlm = continuous[
        continuous$method == "llm" &
            continuous$metric == "overallScore",
        ,
        drop = FALSE
    ]

    expect_equal(nrow(overallLlm), 1)
    expect_equal(overallLlm$n, 3)
    expect_equal(overallLlm$mean, 95)
    expect_equal(overallLlm$range, 0)
})

test_that("stability returns an overall summary for overallScore when available", {
    scores = makeExampleWmfmScoresForStability()

    result = stability(scores)

    expect_type(result$overallSummary, "list")
    expect_true("overallScore" %in% names(result$overallSummary))
    expect_s3_class(result$overallSummary$overallScore, "data.frame")
    expect_true(all(result$overallSummary$overallScore$metric == "overallScore"))
})

test_that("stability skips metrics that are absent from the long data", {
    scores = makeExampleWmfmScoresForStability()
    scores$.testLongDf$comparisonStructureClear = NULL

    result = stability(scores)

    expect_false("comparisonStructureClear" %in% result$ordinalStability$metric)
})

test_that("stability drops all-NA values within a metric", {
    scores = makeExampleWmfmScoresForStability()
    scores$.testLongDf$clarityAdequate[scores$.testLongDf$method == "llm"] = NA

    result = stability(scores)
    clarityRows = result$ordinalStability[
        result$ordinalStability$metric == "clarityAdequate",
        ,
        drop = FALSE
    ]

    expect_equal(nrow(clarityRows), 1)
    expect_equal(clarityRows$method, "deterministic")
})
