test_that("diagnose includes evidenceSummary", {
  scores = makeMockScores(
    detValues = c(0, 0, 0),
    llmValues = c(2, 2, 2)
  )

  runs = makeMockRuns(
    explanations = c("about two points higher", "increase by 2", "roughly two units higher"),
    effectScaleClaim = c("not_stated", "not_stated", "additive"),
    percentLanguageMention = c(FALSE, FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, TRUE, TRUE),
    conditionalLanguageMention = c(FALSE, FALSE, FALSE)
  )

  dx = diagnose(scores, metric = "numericExpressionAdequate", runs = runs)

  expect_true("evidenceSummary" %in% names(dx))
  expect_true(is.data.frame(dx$evidenceSummary))
  expect_equal(dx$evidenceSummary$effectScale_notStated_n, 2)
})
