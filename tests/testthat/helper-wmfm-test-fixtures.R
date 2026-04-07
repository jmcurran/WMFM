# Helper fixtures for WMFM describeField() and stability() tests
#
# These helpers are test-only and create the smallest realistic objects needed
# to exercise S3 dispatch and user-facing behaviour.

`%||%` = function(x, y) {
    if (is.null(x)) {
        y
    } else {
        x
    }
}

makeExampleWmfmRuns = function() {
    structure(
        list(
            runs = list(
                list(
                    runId = 1L,
                    effectDirectionClaim = "increase",
                    ciMention = TRUE,
                    overallScore = 88
                ),
                list(
                    runId = 2L,
                    effectDirectionClaim = "decrease",
                    ciMention = FALSE,
                    overallScore = 74
                )
            )
        ),
        class = "wmfmRuns"
    )
}

makeExampleWmfmScores = function() {
    structure(
        list(
            scores = list(
                deterministic = list(
                    list(
                        runId = 1L,
                        factualScore = 2,
                        overallScore = 92,
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE
                    ),
                    list(
                        runId = 2L,
                        factualScore = 1,
                        overallScore = 81,
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE
                    )
                )
            )
        ),
        class = "wmfmScores"
    )
}

makeExampleWmfmScoresForStability = function() {
    structure(
        list(
            methods = c("deterministic", "llm"),
            runIds = c(1L, 2L, 3L),
            .testLongDf = data.frame(
                method = c(
                    "deterministic", "deterministic", "deterministic",
                    "llm", "llm", "llm"
                ),
                runId = c(1L, 2L, 3L, 1L, 2L, 3L),
                fatalFlawDetected = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
                overallPass = c(TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
                effectDirectionCorrect = c(2, 2, 1, 2, 2, 2),
                clarityAdequate = c(2, 1, 2, 2, 2, 2),
                factualScore = c(2.0, 1.5, 1.0, 2.0, 2.0, 2.0),
                overallScore = c(92, 85, 60, 95, 95, 95),
                stringsAsFactors = FALSE
            )
        ),
        class = "wmfmScores"
    )
}

makeEmptyLongWmfmScores = function() {
    structure(
        list(
            methods = c("deterministic"),
            runIds = integer(0),
            .testLongDf = data.frame()
        ),
        class = "wmfmScores"
    )
}

as.data.frame.wmfmScores = function(x, ..., format = NULL) {
    if (!identical(format, "long")) {
        stop("Test helper only supports format = 'long'.", call. = FALSE)
    }

    x$.testLongDf %||% data.frame()
}
