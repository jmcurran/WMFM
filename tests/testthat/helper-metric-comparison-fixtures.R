makeMetricComparisonScoresFixture = function() {
    scores = list(
        deterministic = list(
            list(
                factualScore = 2,
                clarityScore = 2,
                overallScore = 90,
                llmScored = FALSE,
                llmScoringModel = NA_character_,
                interactionEvidenceAppropriate = "appropriate"
            ),
            list(
                factualScore = 1,
                clarityScore = 1,
                overallScore = 85,
                llmScored = FALSE,
                llmScoringModel = NA_character_,
                interactionEvidenceAppropriate = "appropriate"
            ),
            list(
                factualScore = 2,
                clarityScore = 1,
                overallScore = 88,
                llmScored = FALSE,
                llmScoringModel = NA_character_,
                interactionEvidenceAppropriate = "appropriate"
            )
        ),
        llm = list(
            list(
                factualScore = 2,
                clarityScore = 2,
                overallScore = 91,
                llmScored = TRUE,
                llmScoringModel = "gpt-test",
                interactionEvidenceAppropriate = "appropriate"
            ),
            list(
                factualScore = 2,
                clarityScore = 2,
                overallScore = 84,
                llmScored = TRUE,
                llmScoringModel = "gpt-test",
                interactionEvidenceAppropriate = "appropriate"
            ),
            list(
                factualScore = 1,
                clarityScore = 1,
                overallScore = 88,
                llmScored = TRUE,
                llmScoringModel = "gpt-test",
                interactionEvidenceAppropriate = "appropriate"
            )
        )
    )

    structure(
        list(
            scores = scores,
            methods = c("deterministic", "llm"),
            runIds = c(1, 2, 3)
        ),
        class = "wmfmScores"
    )
}

makeMetricComparisonScoresWithoutLlm = function() {
    scores = makeMetricComparisonScoresFixture()
    scores$scores$llm = NULL
    scores$methods = "deterministic"
    scores
}

makeExampleMetricComparison = function(
    leftMethod = "deterministic",
    rightMethod = "llm"
) {
    pairData = data.frame(
        runId = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
        metric = c(
            rep("factualScore", 3),
            rep("clarityScore", 3),
            rep("overallScore", 3)
        ),
        leftValue = c(
            2, 1, 2,
            2, 1, 1,
            90, 85, 88
        ),
        rightValue = c(
            2, 2, 1,
            2, 2, 1,
            91, 84, 88
        ),
        stringsAsFactors = FALSE
    )

    registry = data.frame(
        metricName = c("factualScore", "clarityScore", "overallScore"),
        label = c("Factual score", "Clarity score", "Overall score"),
        group = c("aggregate", "aggregate", "aggregate"),
        metricType = c("continuous", "continuous", "continuous"),
        stringsAsFactors = FALSE
    )

    binaryAgreement = data.frame()
    ordinalAgreement = data.frame(
        metric = c("factualScore", "clarityScore"),
        meanAbsoluteDifference = c(2 / 3, 1 / 3),
        proportionEqual = c(1 / 3, 2 / 3),
        proportionAdjacent = c(2 / 3, 1 / 3),
        weightedKappa = c(0.4, 0.7),
        stringsAsFactors = FALSE
    )
    continuousAgreement = data.frame(
        metric = "overallScore",
        meanAbsoluteDifference = 2 / 3,
        correlation = 0.9,
        stringsAsFactors = FALSE
    )

    structure(
        list(
            pairData = pairData,
            registry = registry,
            leftMethod = leftMethod,
            rightMethod = rightMethod,
            binaryAgreement = binaryAgreement,
            ordinalAgreement = ordinalAgreement,
            continuousAgreement = continuousAgreement
        ),
        class = "wmfmScoreComparison"
    )
}
