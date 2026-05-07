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

testthat::test_that("developer scoring wraps native app model as wmfmModel", {
  data = data.frame(
    Exam = c(60, 65, 70, 75, 80),
    Test = c(55, 60, 68, 72, 78)
  )
  model = stats::lm(Exam ~ Test, data = data)
  rv = list(
    data = data,
    userDatasetContext = "Synthetic scoring test data.",
    researchQuestion = "Does Test predict Exam?",
    modelEquations = NULL,
    modelExplanation = "Higher Test values are associated with higher Exam values.",
    modelExplanationAudit = NULL
  )
  input = list(
    formula_text = "Exam ~ Test",
    model_type = "lm"
  )

  wmfmModel = buildDeveloperScoringWmfmModel(
    model = model,
    rv = rv,
    input = input,
    explanationText = rv$modelExplanation
  )

  testthat::expect_s3_class(wmfmModel, "wmfmModel")
  testthat::expect_identical(wmfmModel$model, model)
  testthat::expect_equal(wmfmModel$modelType, "lm")
  testthat::expect_equal(paste(deparse(wmfmModel$formula), collapse = " "), "Exam ~ Test")
})

testthat::test_that("developer scoring resolves GLM scale-sensitive model types", {
  data = data.frame(
    Pass = c(0, 0, 1, 0, 1, 1),
    StudyHours = c(1, 2, 3, 4, 5, 6),
    Errors = c(8, 6, 5, 4, 3, 2)
  )
  logisticModel = stats::glm(Pass ~ StudyHours, data = data, family = stats::binomial())
  poissonModel = stats::glm(Errors ~ StudyHours, data = data, family = stats::poisson())

  testthat::expect_equal(resolveDeveloperScoringModelType(logisticModel), "logistic")
  testthat::expect_equal(resolveDeveloperScoringModelType(poissonModel), "poisson")
  testthat::expect_equal(resolveDeveloperScoringModelType(logisticModel, "lm"), "lm")
})

testthat::test_that("developer scoring JSON payload uses stable export schema", {
  gradeObj = list(
    scoreScale = 100,
    meta = list(scored = TRUE),
    scores = list(
      byMethod = list(
        deterministic = list(
          overallScore = 91.2,
          mark = 91.2,
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
          whereMarksLost = data.frame(reason = character(0), stringsAsFactors = FALSE),
          strengths = data.frame(reason = "Clear scale language", stringsAsFactors = FALSE)
        )
      )
    )
  )
  class(gradeObj) = c("wmfmGrade", "list")

  data = data.frame(Exam = c(60, 65, 70), Test = c(55, 60, 68))
  model = stats::lm(Exam ~ Test, data = data)
  rv = list(
    modelExplanation = "Higher Test values are associated with higher Exam values.",
    researchQuestion = "Does Test predict Exam?"
  )
  input = list(formula_text = "Exam ~ Test", model_type = "lm")
  repeatedResult = list(
    totalRuns = 1,
    elapsedSeconds = 2.5,
    createdAt = "2026-05-06 22:00:00",
    runTable = data.frame(
      run = 1,
      scored = TRUE,
      overallScore = 91.2,
      mark = 91.2,
      elapsedSeconds = 2.5,
      status = "scored",
      stringsAsFactors = FALSE
    ),
    runDetails = list(list(
      run = 1,
      status = "scored",
      elapsedSeconds = 2.5,
      explanation = "Higher Test values are associated with higher Exam values.",
      grade = gradeObj
    ))
  )

  payload = buildDeveloperScoringJsonPayload(
    model = model,
    rv = rv,
    input = input,
    gradeObj = gradeObj,
    repeatedResult = repeatedResult
  )
  jsonText = buildDeveloperScoringJsonText(payload)
  parsed = jsonlite::fromJSON(jsonText)

  testthat::expect_equal(payload$schema, "wmfm-developer-scoring-export")
  testthat::expect_equal(payload$schemaVersion, "1.0.0")
  testthat::expect_equal(payload$appState$modelType, "lm")
  testthat::expect_equal(payload$repeated$totalRuns, 1)
  testthat::expect_equal(payload$repeated$runs[[1]]$status, "scored")
  testthat::expect_match(jsonText, '"schema": "wmfm-developer-scoring-export"', fixed = TRUE)
  testthat::expect_equal(parsed$schema, "wmfm-developer-scoring-export")
})


testthat::test_that("developer scoring UI exposes JSON download control", {
  developerObserverText = paste(
    deparse(body(registerDeveloperScoringGradingObservers)),
    collapse = "\n"
  )

  testthat::expect_match(developerObserverText, "developerScoringJsonDownload", fixed = TRUE)
  testthat::expect_match(developerObserverText, "downloadHandler", fixed = TRUE)
  testthat::expect_match(developerObserverText, "buildDeveloperScoringJsonPayload", fixed = TRUE)
})

testthat::test_that("developer scoring JSON sanitises provider S3 objects", {
  providerObject = list(content = "hello", meta = list(cost = 1))
  class(providerObject) = c("ellmer_output", "list")

  payload = list(
    schema = "wmfm-developer-scoring-export",
    providerObject = providerObject
  )

  jsonText = buildDeveloperScoringJsonText(payload)
  parsed = jsonlite::fromJSON(jsonText, simplifyVector = FALSE)

  testthat::expect_equal(parsed$schema, "wmfm-developer-scoring-export")
  testthat::expect_equal(parsed$providerObject$content, "hello")
  testthat::expect_equal(parsed$providerObject$meta$cost, 1)
})

testthat::test_that("numeric-only developer scoring marks factor criteria not applicable", {
  data = data.frame(
    Exam = c(60, 65, 70, 75, 80),
    Test = c(55, 60, 68, 72, 78)
  )
  model = stats::lm(Exam ~ Test, data = data)
  rv = list(
    data = data,
    userDatasetContext = "Synthetic scoring test data.",
    researchQuestion = "Does Test predict Exam?",
    modelEquations = NULL,
    modelExplanation = paste(
      "Higher Test values are associated with higher Exam values.",
      "The fitted line gives an additive change in Exam for each one-unit change in Test."
    ),
    modelExplanationAudit = NULL
  )
  input = list(
    formula_text = "Exam ~ Test",
    model_type = "lm"
  )

  gradeObj = scoreDeveloperExplanation(
    model = model,
    rv = rv,
    input = input,
    explanationText = rv$modelExplanation,
    method = "deterministic"
  )
  metricTable = buildDeveloperScoringMetricTable(gradeObj)

  factorRows = metricTable$label %in% c(
    "Reference group handled correctly",
    "Reference-group coverage adequate",
    "Comparison structure clear"
  )

  testthat::expect_true(all(metricTable$status[factorRows] == "not_applicable"))
  testthat::expect_true(all(metricTable$maxValue[factorRows] == 0))
  testthat::expect_true(all(metricTable$marksLost[factorRows] == 0))
})
