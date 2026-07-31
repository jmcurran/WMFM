testthat::test_that("Stage 49.5 gives logistic individual questions probability-first limits", {
  data = data.frame(
    Y = c(0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0),
    X = c(-2.0, -1.5, -1.0, -0.5, -0.2, 0.0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8)
  )
  model = stats::glm(Y ~ X, data = data, family = stats::binomial())
  question = "What is my chance of Y = 1 if X = 0.5?"
  attr(model, "wmfm_research_question_objective") = buildResearchQuestionObjective(model, question)

  objective = attr(model, "wmfm_research_question_objective")
  block = buildQuestionAwareExplanationPromptBlock(model)
  answer = prependDeterministicResearchQuestionAnswer("Supporting explanation.", model)

  testthat::expect_identical(objective$archetype, "individual_prediction")
  testthat::expect_identical(objective$predictionPayload$predictionResult$status, "ok")
  testthat::expect_equal(objective$predictionPayload$predictionResult$resolvedPredictorValues$X, 0.5)
  testthat::expect_match(block, "fitted probability", ignore.case = TRUE)
  testthat::expect_match(block, "binary", ignore.case = TRUE)
  testthat::expect_match(block, "do not invent or imply a continuous prediction interval", ignore.case = TRUE)
  testthat::expect_match(answer, "predicts a probability", fixed = TRUE)
})

testthat::test_that("Stage 49.5 gives Poisson questions expected-count limits", {
  data = data.frame(
    Y = c(1, 2, 1, 3, 2, 4, 3, 5, 4, 6),
    X = 0:9
  )
  model = stats::glm(Y ~ X, data = data, family = stats::poisson())
  question = "What count would I get if X = 5?"
  attr(model, "wmfm_research_question_objective") = buildResearchQuestionObjective(model, question)

  objective = attr(model, "wmfm_research_question_objective")
  block = buildQuestionAwareExplanationPromptBlock(model)
  answer = prependDeterministicResearchQuestionAnswer("Supporting explanation.", model)

  testthat::expect_identical(objective$archetype, "individual_prediction")
  testthat::expect_match(block, "expected count", ignore.case = TRUE)
  testthat::expect_match(block, "not the exact count", ignore.case = TRUE)
  testthat::expect_match(answer, "predicts an expected count", fixed = TRUE)
})

testthat::test_that("Stage 49.5 compares logistic probabilities on the response scale", {
  data = data.frame(
    Y = c(0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0),
    X = c(-2.0, -1.5, -1.0, -0.5, -0.2, 0.0, 0.3, 0.6, 0.9, 1.2, 1.5, 1.8)
  )
  model = stats::glm(Y ~ X, data = data, family = stats::binomial())
  question = "Compare the expected probability for X = -0.5 versus X = 1.0."
  objective = buildResearchQuestionObjective(model, question)
  attr(model, "wmfm_research_question_objective") = objective

  testthat::expect_identical(objective$answerPayload$status, "ok")
  testthat::expect_identical(objective$answerPayload$modelFamily, "binomial")
  testthat::expect_identical(objective$answerPayload$responseDescription, "probability")
  testthat::expect_identical(objective$answerPayload$intervalMethod, "response_scale_delta_method")
  testthat::expect_true(objective$answerPayload$difference >= -1)
  testthat::expect_true(objective$answerPayload$difference <= 1)

  answer = prependDeterministicResearchQuestionAnswer("Supporting explanation.", model)
  testthat::expect_match(answer, "predicted probability", fixed = TRUE)
  testthat::expect_match(answer, "not guaranteed binary outcomes", fixed = TRUE)
})

testthat::test_that("Stage 49.5 compares Poisson expected counts on the response scale", {
  data = data.frame(
    Y = c(1, 2, 1, 3, 2, 4, 3, 5, 4, 6),
    X = 0:9
  )
  model = stats::glm(Y ~ X, data = data, family = stats::poisson())
  question = "Compare the expected count for X = 2 versus X = 7."
  objective = buildResearchQuestionObjective(model, question)
  attr(model, "wmfm_research_question_objective") = objective

  testthat::expect_identical(objective$answerPayload$status, "ok")
  testthat::expect_identical(objective$answerPayload$modelFamily, "poisson")
  testthat::expect_identical(objective$answerPayload$responseDescription, "expected_count")
  testthat::expect_gt(objective$answerPayload$rightExpectedResponse, objective$answerPayload$leftExpectedResponse)

  answer = prependDeterministicResearchQuestionAnswer("Supporting explanation.", model)
  testthat::expect_match(answer, "expected count", fixed = TRUE)
  testthat::expect_match(answer, "not exact future counts", fixed = TRUE)
})
