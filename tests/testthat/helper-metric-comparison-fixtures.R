makeMetricComparisonScoresFixture = function() {
    deterministic = list(
        list(
            factualScore = 2,
            clarityScore = 2,
            overallScore = 95,
            fatalFlawDetected = FALSE,
            interactionEvidenceAppropriate = "appropriate",
            llmScored = FALSE,
            primaryScoringMethod = "deterministic"
        ),
        list(
            factualScore = 1,
            clarityScore = 1,
            overallScore = 85,
            fatalFlawDetected = FALSE,
            interactionEvidenceAppropriate = "appropriate",
            llmScored = FALSE,
            primaryScoringMethod = "deterministic"
        ),
        list(
            factualScore = 0,
            clarityScore = 2,
            overallScore = 70,
            fatalFlawDetected = TRUE,
            interactionEvidenceAppropriate = "too_strong",
            llmScored = FALSE,
            primaryScoringMethod = "deterministic"
        )
    )

    llm = list(
        list(
            factualScore = 2,
            clarityScore = 2,
            overallScore = 95,
            fatalFlawDetected = FALSE,
            interactionEvidenceAppropriate = "appropriate",
            llmScored = TRUE,
            llmScoringModel = "gpt-test",
            primaryScoringMethod = "llm"
        ),
        list(
            factualScore = 2,
            clarityScore = 2,
            overallScore = 90,
            fatalFlawDetected = FALSE,
            interactionEvidenceAppropriate = "appropriate",
            llmScored = TRUE,
            llmScoringModel = "gpt-test",
            primaryScoringMethod = "llm"
        ),
        list(
            factualScore = 1,
            clarityScore = 2,
            overallScore = 75,
            fatalFlawDetected = FALSE,
            interactionEvidenceAppropriate = "too_weak",
            llmScored = TRUE,
            llmScoringModel = "gpt-test",
            primaryScoringMethod = "llm"
        )
    )

    structure(
        list(
            methods = c("deterministic", "llm"),
            runIds = c(1L, 2L, 3L),
            scores = list(
                deterministic = deterministic,
                llm = llm
            )
        ),
        class = "wmfmScores"
    )
}

makeMetricComparisonScoresWithMissingValues = function() {
    scores = makeMetricComparisonScoresFixture()
    scores$scores$deterministic[[2]]$clarityScore = NA_real_
    scores$scores$llm[[2]]$clarityScore = 2
    scores$scores$deterministic[[3]]$overallScore = NA_real_
    scores$scores$llm[[3]]$overallScore = 75
    scores
}

makeMetricComparisonScoresWithoutLlm = function() {
    scores = makeMetricComparisonScoresFixture()
    scores$scores$llm = NULL
    scores$methods = "deterministic"
    scores
}

makeMetricComparisonObject = function() {
    pairData = data.frame(
        runId = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L),
        metric = c(
            rep("factualScore", 3),
            rep("clarityScore", 3),
            rep("overallScore", 3)
        ),
        leftValue = c(2, 1, 0, 2, 1, 2, 95, 85, 70),
        rightValue = c(2, 2, 1, 2, 2, 2, 95, 90, 75),
        stringsAsFactors = FALSE
    )

    registry = data.frame(
        metricName = c("factualScore", "clarityScore", "overallScore"),
        label = c("Factual score", "Clarity score", "Overall score"),
        group = c("aggregate", "aggregate", "aggregate"),
        metricType = c("ordinal", "ordinal", "continuous"),
        stringsAsFactors = FALSE
    )

    ordinalAgreement = data.frame(
        metric = c("factualScore", "clarityScore"),
        meanAbsoluteDifference = c(2 / 3, 1 / 3),
        proportionEqual = c(1 / 3, 2 / 3),
        proportionAdjacent = c(2 / 3, 1 / 3),
        weightedKappa = c(0.25, 0.50),
        stringsAsFactors = FALSE
    )

    continuousAgreement = data.frame(
        metric = "overallScore",
        meanAbsoluteDifference = 10 / 3,
        correlation = 1,
        stringsAsFactors = FALSE
    )

    binaryAgreement = data.frame(
        metric = character(0),
        meanAbsoluteDifference = numeric(0),
        proportionEqual = numeric(0),
        stringsAsFactors = FALSE
    )

    structure(
        list(
            leftMethod = "deterministic",
            rightMethod = "llm",
            pairData = pairData,
            registry = registry,
            ordinalAgreement = ordinalAgreement,
            binaryAgreement = binaryAgreement,
            continuousAgreement = continuousAgreement
        ),
        class = "wmfmScoreComparison"
    )
}

makeMetricComparisonObjectWithoutDeterministic = function() {
    comparison = makeMetricComparisonObject()
    comparison$leftMethod = "baseline"
    comparison$rightMethod = "llm"
    comparison
}

makeMetricComparisonObjectWithEmptyPairData = function() {
    comparison = makeMetricComparisonObject()
    comparison$pairData = data.frame()
    comparison
}

makeMetricComparisonObjectForSummaryWithMissingAgreement = function() {
    comparison = makeMetricComparisonObject()
    comparison$ordinalAgreement = data.frame()
    comparison$continuousAgreement = data.frame()
    comparison
}
