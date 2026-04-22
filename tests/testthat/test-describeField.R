test_that("describeField dispatches to the wmfmRuns method", {
    runs = makeExampleWmfmRuns()

    result = describeField(runs, field = "Direction claim", format = "list")

    expect_type(result, "list")
    expect_equal(result$canonicalName, "effectDirectionClaim")
    expect_equal(result$title, "Effect direction claim")
})

test_that("describeField dispatches to the wmfmScores method", {
    scores = makeExampleWmfmScores()

    result = describeField(scores, field = "Overall score", format = "list")

    expect_type(result, "list")
    expect_equal(result$canonicalName, "overallScore")
    expect_equal(result$group, "aggregate score")
})

test_that("describeField errors cleanly for an unsupported class", {
    expect_error(
        describeField(mtcars, field = "mpg"),
        regexp = "no applicable method"
    )
})

test_that("describeField.wmfmRuns rejects empty runs lists", {
    runs = structure(list(runs = list()), class = "wmfmRuns")

    expect_error(
        describeField(runs, field = "effectDirectionClaim"),
        regexp = "x\\$runs.*non-empty list"
    )
})

test_that("describeField.wmfmRuns rejects unnamed run records", {
    runs = structure(
        list(runs = list(list("increase", TRUE))),
        class = "wmfmRuns"
    )

    expect_error(
        describeField(runs, field = "effectDirectionClaim"),
        regexp = "named run-record lists"
    )
})

test_that("describeField.wmfmRuns errors when a resolved field is not stored", {
    runs = makeExampleWmfmRuns()

    expect_error(
        describeField(runs, field = "overallPass", format = "list"),
        regexp = "not stored in this `wmfmRuns` object"
    )
})

test_that("describeField.wmfmRuns accepts aliases and pretty labels", {
    runs = makeExampleWmfmRuns()

    fromAlias = describeField(runs, field = "effectDirection", format = "list")
    fromPretty = describeField(runs, field = "Direction claim", format = "list")

    expect_equal(fromAlias$canonicalName, "effectDirectionClaim")
    expect_equal(fromPretty$canonicalName, "effectDirectionClaim")
})

test_that("describeField.wmfmScores rejects missing scores storage", {
    scores = structure(list(scores = NULL), class = "wmfmScores")

    expect_error(
        describeField(scores, field = "overallScore"),
        regexp = "valid `scores` element"
    )
})

test_that("describeField.wmfmScores rejects empty stored score records", {
    scores = structure(list(scores = list(deterministic = list())), class = "wmfmScores")

    expect_error(
        describeField(scores, field = "overallScore"),
        regexp = "does not contain any stored score records"
    )
})

test_that("describeField.wmfmScores rejects malformed score records", {
    scores = structure(
        list(scores = list(deterministic = list(list(1, 2, 3)))),
        class = "wmfmScores"
    )

    expect_error(
        describeField(scores, field = "overallScore"),
        regexp = "Stored score records must be named lists"
    )
})

test_that("describeField.wmfmScores errors when a resolved field is not stored", {
    scores = makeExampleWmfmScores()

    expect_error(
        describeField(scores, field = "CI mention", format = "list"),
        regexp = "not stored in this `wmfmScores` object"
    )
})

test_that("describeWmfmField resolves canonical names, aliases, and labels consistently", {
    canonical = describeWmfmField("effectDirectionClaim", format = "list")
    alias = describeWmfmField("effectDirection", format = "list")
    pretty = describeWmfmField("Direction claim", format = "list")

    expect_equal(alias$canonicalName, canonical$canonicalName)
    expect_equal(pretty$canonicalName, canonical$canonicalName)
})

test_that("describeWmfmField can suppress examples and aliases", {
    result = describeWmfmField(
        "Direction claim",
        format = "list",
        includeExamples = FALSE,
        includeAliases = FALSE
    )

    expect_true(is.na(result$examplesHigh))
    expect_true(is.na(result$examplesLow))
    expect_length(result$aliases, 0)
    expect_length(result$prettyLabels, 0)
})

test_that("describeWmfmField returns a one-row data frame when requested", {
    result = describeWmfmField("overallScore", format = "data.frame")

    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$canonicalName, "overallScore")
    expect_equal(result$group, "aggregate score")
})

test_that("describeWmfmField returns text invisibly and prints a readable description", {
    output = capture.output({
        result = describeWmfmField("CI mention", format = "text")
    })

    expect_true(any(grepl("Field: ciMention", output, fixed = TRUE)))
    expect_true(any(grepl("Title: Confidence-interval mention", output, fixed = TRUE)))
    expect_type(result, "character")
    expect_true(any(grepl("What it measures:", result, fixed = TRUE)))
})

test_that("describeWmfmField errors for an unknown field", {
    expect_error(
        describeWmfmField("notARealField", format = "list"),
        regexp = "Unknown WMFM field"
    )
})

test_that("describeWmfmField errors for empty field input", {
    expect_error(
        describeWmfmField("   ", format = "list"),
        regexp = "single non-empty character string"
    )
})
