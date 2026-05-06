testthat::test_that("developer scoring summary table displays scored grade values", {
  gradeObj = list(
    scoreScale = 10,
    scores = list(
      byMethod = list(
        deterministic = list(
          overallScore = 82.345,
          mark = 8.2345,
          metricSummary = data.frame(
            label = "Overall score",
            studentValue = 82.345,
            maxValue = 100,
            marksLost = 17.655,
            reason = "Some marks were lost.",
            stringsAsFactors = FALSE
          )
        )
      )
    ),
    feedback = list(byMethod = list()),
    meta = list(scored = TRUE)
  )
  class(gradeObj) = c("wmfmGrade", "list")

  summaryTable = buildDeveloperScoringSummaryTable(gradeObj)

  testthat::expect_equal(summaryTable$method, "deterministic")
  testthat::expect_equal(summaryTable$overallScore, 82.34)
  testthat::expect_equal(summaryTable$mark, 8.23)
  testthat::expect_true(summaryTable$scored)
})

testthat::test_that("developer scoring metric table keeps readable diagnostic columns", {
  gradeObj = list(
    scores = list(
      byMethod = list(
        deterministic = list(
          metricSummary = data.frame(
            metric = "overallScore",
            label = "Overall score",
            studentValue = 90,
            maxValue = 100,
            marksLost = 10,
            reason = "Minor loss.",
            internalColumn = "ignored",
            stringsAsFactors = FALSE
          )
        )
      )
    ),
    feedback = list(byMethod = list()),
    meta = list()
  )
  class(gradeObj) = c("wmfmGrade", "list")

  metricTable = buildDeveloperScoringMetricTable(gradeObj)

  testthat::expect_equal(
    names(metricTable),
    c("label", "studentValue", "maxValue", "marksLost", "reason")
  )
  testthat::expect_equal(metricTable$label, "Overall score")
})

testthat::test_that("developer scoring loss table handles missing feedback safely", {
  gradeObj = list(
    scores = list(byMethod = list()),
    feedback = list(byMethod = list()),
    meta = list()
  )
  class(gradeObj) = c("wmfmGrade", "list")

  lossTable = buildDeveloperScoringLossTable(gradeObj, "whereMarksLost")

  testthat::expect_s3_class(lossTable, "data.frame")
  testthat::expect_equal(nrow(lossTable), 0)
})

testthat::test_that("UI and server register developer scoring controls", {
  appUiText = paste(deparse(body(appUI)), collapse = "\n")
  appServerText = paste(deparse(body(appServer)), collapse = "\n")
  scoringServerText = paste(
    deparse(body(registerDeveloperScoringGradingObservers)),
    collapse = "\n"
  )

  testthat::expect_match(appUiText, '"Scoring & Grading"', fixed = TRUE)
  testthat::expect_match(appUiText, 'uiOutput("developerScoringGradingUi")', fixed = TRUE)
  testthat::expect_match(appServerText, "registerDeveloperScoringGradingObservers", fixed = TRUE)
  testthat::expect_match(scoringServerText, 'inputId = "scoreCurrentExplanationBtn"', fixed = TRUE)
  testthat::expect_match(scoringServerText, 'method = "deterministic"', fixed = TRUE)
})
