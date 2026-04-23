test_that("computeDiagnosisPriorityScore ranks higher disagreement first", {
    df = data.frame(
        disagreementRate = c(0.9, 0.2),
        meanLlmMinusDet = c(2, 0.1),
        directionConsistency = c(1, 0.5),
        detConstant = c(TRUE, FALSE),
        llmConstant = c(TRUE, FALSE)
    )

    scores = computeDiagnosisPriorityScore(df)

    expect_true(scores[1] > scores[2])
})
