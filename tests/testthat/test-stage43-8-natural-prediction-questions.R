testthat::test_that("Stage 43.8 expected-value wording is classified as prediction", {
  prompts = c(
    "What is the expected final exam mark for students who attend class regularly and score 15 out of 20 on the test?",
    "If I buy a 1.0 carat diamond with cut Ideal, color G, and clarity VS1, what price should I expect to pay?",
    "What is the expected price of diamonds weighing 1.0 carat with cut Ideal, color G, and clarity VS1?"
  )

  for (prompt in prompts) {
    out = classifyModelFollowupQuestion(prompt)
    testthat::expect_identical(out$category, "prediction_request")
  }
})

testthat::test_that("Stage 43.8 regular attendance resolves to the positive binary level", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("No", "No", "Yes", "Yes", "Yes", "Yes"), levels = c("No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  question = "What mark would you predict I would get if I attended class regularly and scored 15 out of 20 on the test?"

  out = computeLmModelQuestionPrediction(model, question)

  testthat::expect_identical(out$status, "ok")
  testthat::expect_identical(out$resolvedPredictorValues$Attend, "Yes")
  testthat::expect_equal(out$resolvedPredictorValues$Test, 15)
})

testthat::test_that("Stage 43.8 named single-letter factor levels are resolved locally", {
  out = matchNamedFactorLevelCandidate(
    predictor = "color",
    modelLevels = c("D", "E", "F", "G", "H", "I", "J"),
    text = "a 1.0 carat diamond with cut Ideal, color G, and clarity VS1"
  )

  testthat::expect_identical(out, "G")
})

testthat::test_that("Stage 43.8 natural source values resolve transformed predictors", {
  out = extractNaturalTransformedPredictionValue(
    predictor = "log(carat)",
    text = "What price would you predict for a 1.0 carat diamond?"
  )

  testthat::expect_equal(as.numeric(out), 0)
})

testthat::test_that("Stage 43.8 deterministic prediction wording is less technical", {
  df = data.frame(
    Exam = c(45, 54, 62, 71, 80, 88),
    Test = c(8, 10, 12, 14, 16, 18),
    Attend = factor(c("No", "No", "Yes", "Yes", "Yes", "Yes"), levels = c("No", "Yes"))
  )
  model = stats::lm(Exam ~ Attend + Test, data = df)
  question = "What mark would you predict I would get if Attend = Yes and Test = 15?"
  payload = classifyModelFollowupQuestion(question)
  payload = enrichFollowupPayloadWithLmPrediction(model, payload)
  attr(model, "wmfm_model_followup_payload") = payload

  out = buildDeterministicFollowupAnswer(model)

  testthat::expect_match(out, "For one individual with these characteristics", fixed = TRUE)
  testthat::expect_no_match(out, "individual outcome with these predictor values", fixed = TRUE)
  testthat::expect_match(out, "people or cases with these characteristics", fixed = TRUE)
})

testthat::test_that("Stage 43.8 prediction prompt forbids invented outcome thresholds", {
  payload = classifyModelFollowupQuestion(
    "Will I do well if Attend = Yes and Test = 15?"
  )
  out = buildFollowupExplanationControlPromptBlock(payload)

  testthat::expect_match(out, "Do not invent a pass mark", fixed = TRUE)
})
