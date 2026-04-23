test_that("print.wmfmMetricDiagnosis prints summary without error", {
  scores = makeMockScores(
    detValues = c(0, 0, 0),
    llmValues = c(2, 2, 2)
  )

  runs = makeMockRuns(
    explanations = c("a", "b", "c")
  )

  dx = diagnose(scores, metric = "numericExpressionAdequate", runs = runs)

  expect_invisible(print(dx))
})
