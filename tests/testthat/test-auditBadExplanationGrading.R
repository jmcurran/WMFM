test_that("auditBadExplanationGrading flags high-mark bad explanations", {
  makeGrade = function(mark, metricLosses = character(0)) {
    lossesDf = if (length(metricLosses) < 1) {
      data.frame(
        metric = character(0),
        label = character(0),
        marksLost = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        metric = metricLosses,
        label = metricLosses,
        marksLost = rep(1, length(metricLosses)),
        stringsAsFactors = FALSE
      )
    }

    structure(
      list(
        scores = list(
          byMethod = list(
            deterministic = list(
              mark = mark,
              metricSummary = data.frame(
                metric = c("factualScore", "clarityScore"),
                label = c("Factual score", "Clarity score"),
                studentValue = c(1, 1),
                maxValue = c(2, 2),
                stringsAsFactors = FALSE
              )
            )
          )
        ),
        feedback = list(
          byMethod = list(
            deterministic = list(
              whereMarksLost = lossesDf
            )
          )
        )
      ),
      class = "wmfmGrade"
    )
  }

  badGrades = structure(
    list(
      grades = list(
        effectDirectionError = makeGrade(9.5, character(0)),
        causalInferenceError = makeGrade(4.0, "factualScore")
      )
    ),
    class = "wmfmGradeListObj"
  )

  goodGrade = makeGrade(8.8, c("factualScore", "clarityScore"))

  out = auditBadExplanationGrading(
    badGrades,
    goodGrade = goodGrade,
    method = "deterministic",
    minExpectedDrop = 1,
    flaggedThreshold = 9,
    expectedMetrics = list(
      effectDirectionError = c("factualScore"),
      causalInferenceError = c("factualScore")
    )
  )

  expect_s3_class(out, "wmfmBadExplanationAudit")
  expect_true("effectDirectionError" %in% out$flagged$explanationName)
  expect_false("causalInferenceError" %in% out$flagged$explanationName)
})

test_that("auditBadExplanationGrading flags missed expected metrics", {
  makeGrade = function(mark, metricLosses = character(0)) {
    lossesDf = if (length(metricLosses) < 1) {
      data.frame(
        metric = character(0),
        label = character(0),
        marksLost = numeric(0),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(
        metric = metricLosses,
        label = metricLosses,
        marksLost = rep(1, length(metricLosses)),
        stringsAsFactors = FALSE
      )
    }

    structure(
      list(
        scores = list(
          byMethod = list(
            deterministic = list(
              mark = mark,
              metricSummary = data.frame(
                metric = c("factualScore", "clarityScore"),
                label = c("Factual score", "Clarity score"),
                studentValue = c(1, 1),
                maxValue = c(2, 2),
                stringsAsFactors = FALSE
              )
            )
          )
        ),
        feedback = list(
          byMethod = list(
            deterministic = list(
              whereMarksLost = lossesDf
            )
          )
        )
      ),
      class = "wmfmGrade"
    )
  }

  badGrades = structure(
    list(
      grades = list(
        logicalContradiction = makeGrade(8.7, "clarityScore")
      )
    ),
    class = "wmfmGradeListObj"
  )

  goodGrade = makeGrade(8.8, c("factualScore", "clarityScore"))

  out = auditBadExplanationGrading(
    badGrades,
    goodGrade = goodGrade,
    expectedMetrics = list(
      logicalContradiction = c("factualScore", "clarityScore")
    )
  )

  expect_equal(out$details$logicalContradiction$expectedMissed, "factualScore")
  expect_true(out$flagged$missedExpectedMetricCount[1] > 0)
})
