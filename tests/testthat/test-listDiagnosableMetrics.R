test_that("listDiagnosableMetrics finds shared score fields", {
  scores = structure(
    list(
      scores = list(
        deterministic = list(
          list(metricA = 0, metricB = 1, primaryScoringMethod = "deterministic"),
          list(metricA = 1, metricB = 2, primaryScoringMethod = "deterministic")
        ),
        llm = list(
          list(metricA = 2, metricB = 1, primaryScoringMethod = "llm"),
          list(metricA = 2, metricB = 2, primaryScoringMethod = "llm")
        )
      )
    ),
    class = "wmfmScores"
  )

  out = listDiagnosableMetrics(scores)

  expect_equal(out, c("metricA", "metricB"))
})
