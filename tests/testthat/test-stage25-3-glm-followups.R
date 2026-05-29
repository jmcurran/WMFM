testthat::test_that("Stage 25.3 GLM follow-up examples use natural parseable in-range values", {
  exampleNames = c(
    "test-20-B01F-followup",
    "test-21-B11F-followup",
    "test-22-P01F-followup",
    "test-23-P11F-followup"
  )

  developerExamples = listWMFMExamples(includeTestExamples = TRUE)
  visibleExamples = listWMFMExamples(includeTestExamples = FALSE)
  testthat::expect_true(all(exampleNames %in% developerExamples))
  testthat::expect_false(any(exampleNames %in% visibleExamples))

  questions = setNames(
    vapply(exampleNames, function(exampleName) {
      loadExampleSpec(exampleName)$followupQuestion
    }, character(1)),
    exampleNames
  )

  testthat::expect_match(questions[["test-20-B01F-followup"]], "assignment mark", fixed = TRUE)
  testthat::expect_match(questions[["test-20-B01F-followup"]], "Assign = 15", fixed = TRUE)
  testthat::expect_false(grepl("Assign = 80", questions[["test-20-B01F-followup"]], fixed = TRUE))

  testthat::expect_match(questions[["test-21-B11F-followup"]], "I attend regularly", fixed = TRUE)
  testthat::expect_match(questions[["test-21-B11F-followup"]], "Attend = Yes", fixed = TRUE)
  testthat::expect_match(questions[["test-21-B11F-followup"]], "Test = 10", fixed = TRUE)

  testthat::expect_match(questions[["test-22-P01F-followup"]], "smaller earthquake", fixed = TRUE)
  testthat::expect_match(questions[["test-22-P01F-followup"]], "Magnitude = 3", fixed = TRUE)
  testthat::expect_false(grepl("Magnitude = 6.25", questions[["test-22-P01F-followup"]], fixed = TRUE))

  testthat::expect_match(questions[["test-23-P11F-followup"]], "Washington", fixed = TRUE)
  testthat::expect_match(questions[["test-23-P11F-followup"]], "Locn = WA", fixed = TRUE)
  testthat::expect_match(questions[["test-23-P11F-followup"]], "Magnitude = 3", fixed = TRUE)
  testthat::expect_false(grepl("Magnitude = 6.25", questions[["test-23-P11F-followup"]], fixed = TRUE))
})

testthat::test_that("Stage 25.3 natural-language GLM follow-ups parse deterministically", {
  logisticAssign = loadExampleSpec("test-20-B01F-followup")$followupQuestion
  logisticTwoPredictor = loadExampleSpec("test-21-B11F-followup")$followupQuestion
  poissonMagnitude = loadExampleSpec("test-22-P01F-followup")$followupQuestion
  poissonMagnitudeLocn = loadExampleSpec("test-23-P11F-followup")$followupQuestion


  payloads = lapply(
    c(logisticAssign, logisticTwoPredictor, poissonMagnitude, poissonMagnitudeLocn),
    classifyModelFollowupQuestion
  )
  testthat::expect_true(all(vapply(payloads, function(payload) identical(payload$category, "prediction_request"), logical(1))))

  pairsAssign = extractPredictionAssignmentPairs(logisticAssign)
  testthat::expect_identical(pairsAssign$Assign, "15")

  pairsTwoPredictor = extractPredictionAssignmentPairs(logisticTwoPredictor)
  testthat::expect_identical(pairsTwoPredictor$Attend, "Yes")
  testthat::expect_identical(pairsTwoPredictor$Test, "10")

  pairsMagnitude = extractPredictionAssignmentPairs(poissonMagnitude)
  testthat::expect_identical(pairsMagnitude$Magnitude, "3")

  pairsMagnitudeLocn = extractPredictionAssignmentPairs(poissonMagnitudeLocn)
  testthat::expect_identical(pairsMagnitudeLocn$Locn, "WA")
  testthat::expect_identical(pairsMagnitudeLocn$Magnitude, "3")
})

testthat::test_that("Stage 25.3 course logistic Assign follow-up predicts an in-range assignment value", {
  testthat::skip_if_not_installed("s20x")
  data(course.df, package = "s20x")
  question = loadExampleSpec("test-20-B01F-followup")$followupQuestion
  model = stats::glm(Pass ~ Assign, data = course.df, family = stats::binomial())

  out = computeGlmModelQuestionPrediction(model, question)
  ref = as.numeric(stats::predict(model, newdata = data.frame(Assign = 15), type = "response"))[1]

  testthat::expect_identical(out$status, "ok")
  testthat::expect_equal(out$resolvedPredictorValues$Assign, 15)
  testthat::expect_true(out$resolvedPredictorValues$Assign >= min(course.df$Assign, na.rm = TRUE))
  testthat::expect_true(out$resolvedPredictorValues$Assign <= max(course.df$Assign, na.rm = TRUE))
  testthat::expect_equal(out$fittedPrediction, ref)
  testthat::expect_true(is.list(out$confidenceInterval))
  testthat::expect_identical(out$predictionIntervalSupported, FALSE)
})

testthat::test_that("Stage 25.3 earthquake follow-ups predict at Magnitude 3", {
  onePredictorInfo = loadExampleSpec("test-22-P01F-followup")
  twoPredictorInfo = loadExampleSpec("test-23-P11F-followup")

  onePredictorModel = stats::glm(Freq ~ Magnitude, data = onePredictorInfo$data, family = stats::poisson())
  twoPredictorModel = stats::glm(Freq ~ Magnitude + Locn, data = twoPredictorInfo$data, family = stats::poisson())

  outMagnitude = computeGlmModelQuestionPrediction(onePredictorModel, onePredictorInfo$followupQuestion)
  testthat::expect_identical(outMagnitude$status, "ok")
  testthat::expect_equal(outMagnitude$resolvedPredictorValues$Magnitude, 3)
  testthat::expect_true(is.list(outMagnitude$confidenceInterval))
  testthat::expect_identical(outMagnitude$predictionIntervalSupported, FALSE)

  outMagnitudeLocn = computeGlmModelQuestionPrediction(twoPredictorModel, twoPredictorInfo$followupQuestion)
  testthat::expect_identical(outMagnitudeLocn$status, "ok")
  testthat::expect_equal(outMagnitudeLocn$resolvedPredictorValues$Magnitude, 3)
  testthat::expect_identical(outMagnitudeLocn$resolvedPredictorValues$Locn, "WA")
  testthat::expect_true(is.list(outMagnitudeLocn$confidenceInterval))
  testthat::expect_match(outMagnitudeLocn$predictionIntervalUnsupportedReason, "not currently supported", fixed = TRUE)
})
