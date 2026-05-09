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

  testthat::expect_false(grepl('tabPanel\\("Scoring & Grading"', appUiText))
  testthat::expect_match(appServerText, "registerDeveloperScoringGradingObservers", fixed = TRUE)
  testthat::expect_match(developerObserverText, "insertTab", fixed = TRUE)
  testthat::expect_match(developerObserverText, "developer_scoring_grading", fixed = TRUE)
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
  testthat::expect_equal(payload$schemaVersion, "1.1.0")
  testthat::expect_equal(payload$appState$modelType, "lm")
  testthat::expect_equal(payload$repeated$totalRuns, 1)
  testthat::expect_equal(payload$repeated$runs[[1]]$status, "scored")
  testthat::expect_match(jsonText, '"schema": "wmfm-developer-scoring-export"', fixed = TRUE)
  testthat::expect_equal(parsed$schema, "wmfm-developer-scoring-export")
})


testthat::test_that("developer scoring metric export uses conditional feedback and metadata", {
  scored = data.frame(
    overallScore = 100,
    factualScore = 2,
    inferenceScore = 2,
    completenessScore = 2,
    clarityScore = 1.667,
    calibrationScore = 2,
    effectDirectionCorrect = 2,
    effectScaleAppropriate = 2,
    referenceGroupHandledCorrectly = NA_real_,
    interactionCoverageAdequate = NA_real_,
    interactionSubstantiveCorrect = NA_real_,
    uncertaintyHandlingAppropriate = 2,
    inferentialRegisterAppropriate = 2,
    mainEffectCoverageAdequate = 2,
    referenceGroupCoverageAdequate = NA_real_,
    clarityAdequate = 1,
    numericExpressionAdequate = 2,
    comparisonStructureClear = NA_real_,
    explanationText = paste(
      "Each one-point increase in Test is associated with a 0.8 point",
      "increase in Exam, with a 95 percent confidence interval."
    ),
    stringsAsFactors = FALSE
  )

  feedback = summariseWmfmGradeLosses(scored, method = "deterministic")
  metricSummary = feedback$metricSummary
  effectRow = metricSummary[metricSummary$metric == "effectScaleAppropriate", , drop = FALSE]
  clarityRow = metricSummary[metricSummary$metric == "clarityScore", , drop = FALSE]
  interactionRow = metricSummary[metricSummary$metric == "interactionCoverageAdequate", , drop = FALSE]

  testthat::expect_equal(effectRow$reason, "The effect was described on an appropriate scale.")
  testthat::expect_equal(clarityRow$studentValue, 1.75)
  testthat::expect_true("confidence" %in% names(metricSummary))
  testthat::expect_true("evidenceStrength" %in% names(metricSummary))
  testthat::expect_equal(effectRow$confidence, "high")
  testthat::expect_equal(interactionRow$confidence, "not_applicable")
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
  testthat::expect_true(all(metricTable$confidence[factorRows] == "not_applicable"))

  lossTable = buildDeveloperScoringLossTable(gradeObj, "whereMarksLost")

  testthat::expect_false(any(lossTable$label %in% c(
    "Reference group handled correctly",
    "Reference-group coverage adequate",
    "Comparison structure clear"
  )))
})


testthat::test_that("developer scoring JSON sanitises classed atomic provider values", {
  providerObject = structure("hello", class = c("ellmer_output", "character"))

  payload = list(
    schema = "wmfm-developer-scoring-export",
    providerObject = providerObject
  )

  jsonText = buildDeveloperScoringJsonText(payload)
  parsed = jsonlite::fromJSON(jsonText, simplifyVector = FALSE)

  testthat::expect_equal(parsed$schema, "wmfm-developer-scoring-export")
  testthat::expect_equal(parsed$providerObject, "hello")
})

testthat::test_that("developer scoring tab is not rendered while developer mode is locked", {
  appUiText = paste(deparse(body(appUI)), collapse = "\n")
  developerObserverText = paste(
    deparse(body(registerDeveloperScoringGradingObservers)),
    collapse = "\n"
  )

  testthat::expect_false(grepl('tabPanel\\("Scoring & Grading"', appUiText))
  testthat::expect_match(developerObserverText, "insertTab", fixed = TRUE)
  testthat::expect_match(developerObserverText, "removeTab", fixed = TRUE)
  testthat::expect_match(developerObserverText, "developer_scoring_grading", fixed = TRUE)
})

testthat::test_that("developer scoring resets when a new example loads", {
  developerObserverText = paste(
    deparse(body(registerDeveloperScoringGradingObservers)),
    collapse = "\n"
  )

  testthat::expect_match(developerObserverText, "resetDeveloperScoringState", fixed = TRUE)
  testthat::expect_match(developerObserverText, "observeEvent(rv$loadedExample", fixed = TRUE)
  testthat::expect_match(developerObserverText, "developerGrade(NULL)", fixed = TRUE)
  testthat::expect_match(developerObserverText, "developerRepeatedResult(NULL)", fixed = TRUE)
})

testthat::test_that("factor group mean comparisons count as effect-scale interpretation", {
  rawRecord = data.frame(
    explanationText = paste(
      "Female students have a predicted mean exam mark of 82,",
      "while male students have a predicted mean exam mark of 63.",
      "Overall, female students achieve higher final exam marks than male students."
    ),
    hasFactorPredictors = TRUE,
    hasInteractionTerms = FALSE,
    comparisonLanguageMention = TRUE,
    effectDirectionClaim = "increase",
    effectScaleClaim = "not_stated",
    referenceGroupMention = FALSE,
    uncertaintyMention = FALSE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    usesDescriptiveOnlyLanguage = FALSE,
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    conditionalLanguageMention = FALSE,
    outcomeMention = TRUE,
    predictorMention = TRUE,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecord, penaliseDuplicates = FALSE)

  testthat::expect_equal(scored$effectScaleAppropriate, 2L)
  testthat::expect_equal(scored$numericExpressionAdequate, 2L)
  testthat::expect_equal(scored$mainEffectCoverageAdequate, 2L)
})


testthat::test_that("non-interaction models mark interaction criteria not applicable", {
  rawRecord = data.frame(
    explanationText = paste(
      "Female students have a predicted mean exam mark of 82,",
      "while male students have a predicted mean exam mark of 63.",
      "Overall, female students achieve higher final exam marks than male students."
    ),
    hasFactorPredictors = TRUE,
    hasInteractionTerms = FALSE,
    comparisonLanguageMention = TRUE,
    effectDirectionClaim = "increase",
    effectScaleClaim = "not_stated",
    referenceGroupMention = FALSE,
    uncertaintyMention = FALSE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    usesDescriptiveOnlyLanguage = FALSE,
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    conditionalLanguageMention = FALSE,
    outcomeMention = TRUE,
    predictorMention = TRUE,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecord, penaliseDuplicates = FALSE)
  feedback = summariseWmfmGradeLosses(scored, method = "deterministic")
  metricSummary = feedback$metricSummary
  interactionRows = metricSummary$metric %in% c(
    "interactionCoverageAdequate",
    "interactionSubstantiveCorrect"
  )

  testthat::expect_true(all(is.na(scored$interactionCoverageAdequate)))
  testthat::expect_true(all(is.na(scored$interactionSubstantiveCorrect)))
  testthat::expect_true(all(metricSummary$status[interactionRows] == "not_applicable"))
  testthat::expect_true(all(metricSummary$maxValue[interactionRows] == 0))
  testthat::expect_true(all(metricSummary$marksLost[interactionRows] == 0))
})


testthat::test_that("interaction models continue to score interaction criteria", {
  rawRecord = data.frame(
    explanationText = paste(
      "The effect of attendance differs by gender.",
      "The attendance slope is steeper for female students than for male students."
    ),
    hasFactorPredictors = TRUE,
    hasInteractionTerms = TRUE,
    interactionMinPValue = 0.01,
    interactionAlpha = 0.05,
    comparisonLanguageMention = TRUE,
    conditionalLanguageMention = TRUE,
    interactionMention = TRUE,
    interactionSubstantiveClaim = "difference_claimed_cautiously",
    effectDirectionClaim = "mixed_or_both",
    effectScaleClaim = "additive",
    referenceGroupMention = TRUE,
    uncertaintyMention = TRUE,
    ciMention = FALSE,
    usesInferentialLanguage = TRUE,
    usesDescriptiveOnlyLanguage = FALSE,
    overclaimDetected = FALSE,
    underclaimDetected = FALSE,
    outcomeMention = TRUE,
    predictorMention = TRUE,
    stringsAsFactors = FALSE
  )

  scored = scoreWmfmRunRecordsCore(rawRecord, penaliseDuplicates = FALSE)

  testthat::expect_false(is.na(scored$interactionCoverageAdequate))
  testthat::expect_false(is.na(scored$interactionSubstantiveCorrect))
  testthat::expect_equal(scored$interactionCoverageAdequate, 2L)
  testthat::expect_equal(scored$interactionSubstantiveCorrect, 2L)
})

testthat::test_that("developer scoring export includes semantic evidence diagnostics when present", {
  semanticEvidence = data.frame(
    field = "effectDirection",
    label = "Effect direction",
    value = "positive",
    evidencePresent = TRUE,
    detail = "Direction inferred from wording.",
    stringsAsFactors = FALSE
  )

  gradeObj = list(
    scoreScale = 100,
    meta = list(scored = TRUE),
    scores = list(
      byMethod = list(
        deterministic = list(
          overallScore = 100,
          mark = 100,
          metricSummary = data.frame(
            label = "Overall score",
            studentValue = 100,
            maxValue = 100,
            marksLost = 0,
            reason = "Correct",
            stringsAsFactors = FALSE
          ),
          semanticEvidence = semanticEvidence
        )
      )
    ),
    feedback = list(
      byMethod = list(
        deterministic = list(
          whereMarksLost = data.frame(reason = character(0), stringsAsFactors = FALSE),
          strengths = data.frame(reason = "Clear", stringsAsFactors = FALSE),
          semanticEvidence = semanticEvidence
        )
      ),
      semanticEvidence = semanticEvidence
    )
  )
  class(gradeObj) = c("wmfmGrade", "list")

  exported = buildDeveloperScoringGradeExport(gradeObj)

  testthat::expect_s3_class(exported$semanticEvidence, "data.frame")
  testthat::expect_equal(exported$semanticEvidence$field, "effectDirection")
  testthat::expect_equal(exported$semanticEvidence$value, "positive")
})

