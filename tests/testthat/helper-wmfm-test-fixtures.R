# Helper fixtures for WMFM describeField() and stability() tests
#
# These helpers are test-only and create small realistic objects needed
# to exercise S3 dispatch and user-facing behaviour.

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
                        factualScore = 2,
                        overallScore = 92,
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE
                    ),
                    list(
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
            scores = list(
                deterministic = list(
                    list(
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE,
                        effectDirectionCorrect = 2,
                        clarityAdequate = 2,
                        comparisonStructureClear = 2,
                        factualScore = 2.0,
                        inferenceScore = 2.0,
                        overallScore = 92
                    ),
                    list(
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE,
                        effectDirectionCorrect = 2,
                        clarityAdequate = 1,
                        comparisonStructureClear = 1,
                        factualScore = 1.5,
                        inferenceScore = 1.5,
                        overallScore = 85
                    ),
                    list(
                        fatalFlawDetected = TRUE,
                        overallPass = FALSE,
                        effectDirectionCorrect = 1,
                        clarityAdequate = 2,
                        comparisonStructureClear = 2,
                        factualScore = 1.0,
                        inferenceScore = 1.0,
                        overallScore = 60
                    )
                ),
                llm = list(
                    list(
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE,
                        effectDirectionCorrect = 2,
                        clarityAdequate = 2,
                        comparisonStructureClear = 2,
                        factualScore = 2.0,
                        inferenceScore = 2.0,
                        overallScore = 95
                    ),
                    list(
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE,
                        effectDirectionCorrect = 2,
                        clarityAdequate = 2,
                        comparisonStructureClear = 2,
                        factualScore = 2.0,
                        inferenceScore = 2.0,
                        overallScore = 95
                    ),
                    list(
                        fatalFlawDetected = FALSE,
                        overallPass = TRUE,
                        effectDirectionCorrect = 2,
                        clarityAdequate = 2,
                        comparisonStructureClear = 2,
                        factualScore = 2.0,
                        inferenceScore = 2.0,
                        overallScore = 95
                    )
                )
            )
        ),
        class = "wmfmScores"
    )
}

makeEmptyWmfmScoresForStability = function() {
    structure(
        list(
            methods = c("deterministic"),
            runIds = integer(0),
            scores = list(deterministic = list())
        ),
        class = "wmfmScores"
    )
}
