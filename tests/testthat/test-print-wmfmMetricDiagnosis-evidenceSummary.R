test_that("print.wmfmMetricDiagnosis prints evidence summary without error", {
  scores = makeMockScores(
    detValues = c(0, 0, 0),
    llmValues = c(2, 2, 2)
  )

  runs = makeMockRuns(
    explanations = c("a", "b", "c"),
    effectScaleClaim = c("not_stated", "not_stated", "additive"),
    percentLanguageMention = c(FALSE, FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, TRUE, TRUE),
    conditionalLanguageMention = c(FALSE, FALSE, FALSE)
  )

  dx = diagnose(scores, metric = "numericExpressionAdequate", runs = runs)

  expect_invisible(print(dx))
})
