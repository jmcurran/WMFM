test_that("print.wmfmScoresDiagnosis prints without error", {
  scores = structure(
    list(
      scores = list(
        deterministic = list(
          list(
            numericExpressionAdequate = 0,
            clarityAdequate = 1
          ),
          list(
            numericExpressionAdequate = 0,
            clarityAdequate = 1
          )
        ),
        llm = list(
          list(
            numericExpressionAdequate = 2,
            clarityAdequate = 1
          ),
          list(
            numericExpressionAdequate = 2,
            clarityAdequate = 2
          )
        )
      )
    ),
    class = "wmfmScores"
  )

  runs = makeMockRuns(
    explanations = c("a", "b"),
    effectScaleClaim = c("not_stated", "additive"),
    percentLanguageMention = c(FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, TRUE),
    conditionalLanguageMention = c(FALSE, FALSE)
  )

  dxAll = diagnose(scores, metric = NULL, runs = runs)

  expect_invisible(print(dxAll))
})
