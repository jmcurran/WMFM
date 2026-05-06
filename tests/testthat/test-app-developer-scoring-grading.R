testthat::test_that("developer scoring summary table displays scored grade values", {
  gradeObj = list(
    scoreScale = 100,
    meta = list(scored = TRUE),
    scores = list(
      byMethod = list(
        deterministic = list(
          overallScore = 82.34,
          mark = 82.34,
          metricSummary = data.frame(
            label = "Scale",
            studentValue = 1,
            maxValue = 1,
            marksLost = 0,
            reason = "Correct",
            stringsAsFactors = FALSE
          )
        )
      )
    ),
    feedback = list(
      byMethod = list(
        deterministic = list(
          whereMarksLost = data.frame(reason = "None", stringsAsFactors = FALSE),
          strengths = data.frame(reason = "Clear", stringsAsFactors = FALSE)
        )
      )
    )
  )
  class(gradeObj) = c("wmfmGrade", "list")

  summaryTable = buildDeveloperScoringSummaryTable(gradeObj)
  metricTable = buildDeveloperScoringMetricTable(gradeObj)
  lossTable = buildDeveloperScoringLossTable(gradeObj, "whereMarksLost")
  objectText = buildDeveloperScoringObjectText(gradeObj)

  testthat::expect_equal(summaryTable$overallScore, 82.34)
  testthat::expect_equal(summaryTable$mark, 82.34)
  testthat::expect_equal(summaryTable$scoreScale, 100)
  testthat::expect_true(summaryTable$scored)
  testthat::expect_equal(metricTable$label, "Scale")
  testthat::expect_equal(lossTable$reason, "None")
  testthat::expect_type(objectText, "character")
})

testthat::test_that("developer repeated scoring progress text includes ETA", {
  progressText = buildDeveloperScoringProgressText(
    completed = 2,
    total = 5,
    elapsedSeconds = 20
  )

  testthat::expect_match(progressText, "2 of 5 runs complete", fixed = TRUE)
  testthat::expect_match(progressText, "Elapsed: 20.0 seconds", fixed = TRUE)
  testthat::expect_match(progressText, "Estimated remaining: 30.0 seconds", fixed = TRUE)
})

testthat::test_that("developer repeated scoring tables summarise run marks", {
  repeatedResult = list(
    totalRuns = 3,
    elapsedSeconds = 12,
    runTable = data.frame(
      run = 1:3,
      scored = c(TRUE, TRUE, FALSE),
      overallScore = c(90, 80, NA),
      mark = c(90, 80, NA),
      elapsedSeconds = c(4, 4, 4),
      status = c("scored", "scored", "no explanation returned"),
      stringsAsFactors = FALSE
    )
  )

  summaryTable = buildDeveloperRepeatedScoringSummaryTable(repeatedResult)
  runTable = buildDeveloperRepeatedScoringRunTable(repeatedResult)

  testthat::expect_equal(summaryTable$requestedRuns, 3)
  testthat::expect_equal(summaryTable$completedRuns, 3)
  testthat::expect_equal(summaryTable$scoredRuns, 2)
  testthat::expect_equal(summaryTable$meanMark, 85)
  testthat::expect_equal(summaryTable$minMark, 80)
  testthat::expect_equal(summaryTable$maxMark, 90)
  testthat::expect_equal(nrow(runTable), 3)
})

testthat::test_that("UI and server register developer scoring controls", {
  appUiText = paste(deparse(body(appUI)), collapse = "\n")
  appServerText = paste(deparse(body(appServer)), collapse = "\n")
  developerObserverText = paste(
    deparse(body(registerDeveloperScoringGradingObservers)),
    collapse = "\n"
  )

  testthat::expect_match(appUiText, 'uiOutput("developerScoringGradingUi")', fixed = TRUE)
  testthat::expect_match(appServerText, "registerDeveloperScoringGradingObservers", fixed = TRUE)
  testthat::expect_match(developerObserverText, "developerScoringRunCount", fixed = TRUE)
  testthat::expect_match(developerObserverText, "runRepeatedScoringBtn", fixed = TRUE)
  testthat::expect_match(developerObserverText, "withProgress", fixed = TRUE)
})
