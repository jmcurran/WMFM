testthat::test_that("prediction-shaped research question is detected conservatively", {
  testthat::expect_true(isPredictionShapedResearchQuestion("What exam mark would a student with Test = 10 get?"))
  testthat::expect_true(isPredictionShapedResearchQuestion("Predict Exam for Test = 10"))
})

testthat::test_that("association-style research questions are not detected as prediction", {
  testthat::expect_false(isPredictionShapedResearchQuestion("Does Exam tend to increase as Test increases?"))
  testthat::expect_false(isPredictionShapedResearchQuestion("Explain the relationship between Test and Exam"))
})

testthat::test_that("explicit predictor assignments in research questions are extracted safely", {
  out = extractPredictionAssignmentPairs("What would expected response be when x = 5?")
  testthat::expect_identical(out$x, "5")
})

testthat::test_that("complete lm prediction-shaped research question gives deterministic payload", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  payload = buildResearchQuestionPredictionPayload(model, "Predict Exam for Test = 10 with confidence interval")
  testthat::expect_identical(payload$source, "research_question")
  testthat::expect_identical(payload$predictionResult$status, "ok")
  testthat::expect_true(is.list(payload$predictionResult$confidenceInterval))
})

testthat::test_that("missing predictor values in research question fail safely", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7), Attend = factor(c("No", "Yes", "Yes", "Yes", "No", "Yes")))
  model = stats::lm(Exam ~ Test + Attend, data = df)

  payload = buildResearchQuestionPredictionPayload(model, "Predict exam for Test = 10")
  testthat::expect_identical(payload$predictionResult$status, "needs_input")
  testthat::expect_identical(payload$predictionResult$reason, "missing_required_predictor_values")
})

testthat::test_that("individual prediction wording in research question requests lm prediction interval", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)

  payload = buildResearchQuestionPredictionPayload(model, "What exam mark would an individual student get for Test = 10 with prediction interval?")
  testthat::expect_identical(payload$predictionResult$predictionType, "individual_prediction_interval")
  testthat::expect_true(is.list(payload$predictionResult$predictionInterval))
})

testthat::test_that("glm prediction interval via research question fails safely", {
  df = data.frame(Y = c(0, 1, 0, 1, 1, 0), X = c(1, 2, 3, 4, 5, 6))
  model = stats::glm(Y ~ X, data = df, family = stats::binomial())

  payload = buildResearchQuestionPredictionPayload(model, "Predict Y for X = 3 with prediction interval")
  testthat::expect_identical(payload$predictionResult$status, "unsupported")
})

testthat::test_that("prompt includes research-question deterministic prediction payload", {
  df = data.frame(Exam = c(42, 58, 81, 86, 35, 72), Test = c(9.1, 13.6, 14.5, 19.1, 8.2, 12.7))
  model = stats::lm(Exam ~ Test, data = df)
  attr(model, "wmfm_research_question") = "Predict Exam for Test = 10"
  attr(model, "wmfm_model_followup_question") = ""
  attr(model, "wmfm_model_followup_payload") = classifyModelFollowupQuestion("")

  prompt = lmToExplanationPrompt(model)
  testthat::expect_match(prompt, "Research question context", fixed = TRUE)
  testthat::expect_match(prompt, "WMFM deterministic prediction payload", fixed = TRUE)
})
