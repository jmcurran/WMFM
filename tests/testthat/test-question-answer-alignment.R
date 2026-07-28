test_that("Stage 49.6 diagnoses aligned prediction-first answers", {
  model = lm(mpg ~ wt, data = mtcars)
  objective = buildResearchQuestionObjective(model, "What mpg will I get if wt = 3?")
  prediction = objective$predictionPayload$predictionResult
  deterministic = prediction$deterministicResponse %||% objective$predictionPayload$deterministicResponse
  answer = paste(
    deterministic,
    "This prediction is uncertain, so the individual prediction interval gives a plausible range.",
    "The supplied weight is 3.",
    sep = "\n\n"
  )
  diagnostic = diagnoseQuestionAnswerAlignment(answer, objective, mode = "standard")
  expect_s3_class(diagnostic, "wmfmQuestionAlignment")
  expect_true(diagnostic$checks[["deterministicFirst"]])
  expect_true(diagnostic$checks[["profileCovered"]])
  expect_true(diagnostic$checks[["noContradictions"]])
})

test_that("Stage 49.6 detects displacement and interval contradictions", {
  data = data.frame(y = c(0, 1, 0, 1, 0, 1, 1, 0), x = c(-2, -1, -0.5, 0, 0.5, 1, 1.5, 2))
  model = glm(y ~ x, family = binomial(), data = data)
  objective = buildResearchQuestionObjective(model, "What is my chance that y = 1 if x = 0.5?")
  prediction = objective$predictionPayload$predictionResult
  deterministic = prediction$deterministicResponse %||% objective$predictionPayload$deterministicResponse
  answer = paste(
    "The coefficient is positive. The continuous prediction interval from 0.2 to 0.8 describes the outcome.",
    deterministic
  )
  diagnostic = diagnoseQuestionAnswerAlignment(answer, objective)
  expect_false(diagnostic$checks[["deterministicFirst"]])
  expect_false(diagnostic$checks[["noContradictions"]])
  expect_true("continuous_interval_for_binary_outcome" %in% diagnostic$contradictions)
})

test_that("Stage 49.6 applies mode-specific length diagnostics", {
  model = lm(mpg ~ wt, data = mtcars)
  objective = buildResearchQuestionObjective(model, "What mpg will I get if wt = 3?")
  longAnswer = paste(rep("prediction uncertainty interval", 80), collapse = " ")
  concise = diagnoseQuestionAnswerAlignment(longAnswer, objective, mode = "concise")
  standard = diagnoseQuestionAnswerAlignment(longAnswer, objective, mode = "standard")
  detailed = diagnoseQuestionAnswerAlignment(longAnswer, objective, mode = "detailed")
  expect_false(concise$checks[["modeAppropriate"]])
  expect_true(standard$checks[["modeAppropriate"]])
  expect_true(detailed$checks[["modeAppropriate"]])
})


test_that("Stage 49.6.1 recognises a probability-first deterministic paragraph without cached response text", {
  data = data.frame(y = c(0, 1, 0, 1, 0, 1, 1, 0), x = c(-2, -1, -0.5, 0, 0.5, 1, 1.5, 2))
  model = glm(y ~ x, family = binomial(), data = data)
  objective = buildResearchQuestionObjective(model, "What is my chance that y = 1 if x = 0.5?")
  prediction = objective$predictionPayload$predictionResult
  fittedText = formatFollowupPredictionNumber(prediction$fittedPrediction)
  answer = paste0(
    "For the follow-up question, using x = 0.5, WMFM predicts a probability for y of ",
    fittedText,
    ". Supporting explanation follows."
  )
  diagnostic = diagnoseQuestionAnswerAlignment(answer, objective)
  expect_true(diagnostic$checks[["deterministicFirst"]])
})
