test_that("diagnose with metric NULL returns wmfmScoresDiagnosis", {
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
          ),
          list(
            numericExpressionAdequate = 0,
            clarityAdequate = 2
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
    explanations = c("a", "b", "c"),
    effectScaleClaim = c("not_stated", "not_stated", "additive"),
    percentLanguageMention = c(FALSE, FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, TRUE, TRUE),
    conditionalLanguageMention = c(FALSE, FALSE, FALSE)
  )

  dxAll = diagnose(scores, metric = NULL, runs = runs)

  expect_s3_class(dxAll, "wmfmScoresDiagnosis")
  expect_true(is.data.frame(dxAll$summaryTable))
  expect_true(is.data.frame(dxAll$flaggedMetrics))
  expect_true(is.list(dxAll$metricDiagnoses))
  expect_true(all(c("numericExpressionAdequate", "clarityAdequate") %in% names(dxAll$metricDiagnoses)))
})

test_that("diagnose all-metrics ranks systematic disagreement highly", {
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
          ),
          list(
            numericExpressionAdequate = 0,
            clarityAdequate = 2
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
    explanations = c("a", "b", "c"),
    effectScaleClaim = c("not_stated", "not_stated", "additive"),
    percentLanguageMention = c(FALSE, FALSE, FALSE),
    comparisonLanguageMention = c(TRUE, TRUE, TRUE),
    conditionalLanguageMention = c(FALSE, FALSE, FALSE)
  )

  dxAll = diagnose(scores, metric = NULL, runs = runs)

  expect_equal(dxAll$flaggedMetrics$metric[[1]], "numericExpressionAdequate")
  expect_true(dxAll$flaggedMetrics$priorityScore[[1]] >= dxAll$flaggedMetrics$priorityScore[[2]])
})
