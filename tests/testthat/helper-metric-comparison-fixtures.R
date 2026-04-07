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

    structure(
        list(
            pairData = pairData,
            registry = registry,
            leftMethod = leftMethod,
            rightMethod = rightMethod
        ),
        class = "wmfmScoreComparison"
    )
}
