testthat::test_that("Stage 49.4 keeps expected responses separate from individual predictions", {
  data = data.frame(Y = c(2, 4, 5, 8, 10, 11), X = 1:6)
  model = stats::lm(Y ~ X, data = data)
  question = "What is the expected response for X = 4 with a confidence interval?"
  attr(model, "wmfm_research_question_objective") = buildResearchQuestionObjective(model, question)

  objective = attr(model, "wmfm_research_question_objective")
  block = buildQuestionAwareExplanationPromptBlock(model)
  answer = prependDeterministicResearchQuestionAnswer("Supporting explanation.", model)

  testthat::expect_identical(objective$archetype, "expected_response")
  testthat::expect_match(block, "expected or average response", fixed = TRUE)
  testthat::expect_match(block, "do not describe its confidence interval as a prediction interval", ignore.case = TRUE)
  testthat::expect_match(answer, "estimated average Y", fixed = TRUE)
  testthat::expect_false(grepl("For an individual", answer, fixed = TRUE))
})

testthat::test_that("Stage 49.4 compares two complete predictor profiles", {
  data = data.frame(Y = c(2, 4, 5, 8, 10, 11), X = 1:6)
  model = stats::lm(Y ~ X, data = data)
  question = "Compare the expected response for X = 2 versus X = 5."
  objective = buildResearchQuestionObjective(model, question)
  attr(model, "wmfm_research_question_objective") = objective

  testthat::expect_identical(objective$archetype, "compare_groups_or_profiles")
  testthat::expect_identical(objective$answerPayload$status, "ok")
  testthat::expect_identical(objective$answerPayload$intervalType, "confidence_interval_for_expected_response_difference")

  answer = prependDeterministicResearchQuestionAnswer("Supporting explanation.", model)
  testthat::expect_match(answer, "second profile is estimated to differ", fixed = TRUE)
  testthat::expect_match(answer, "not an individual prediction interval", fixed = TRUE)
})

testthat::test_that("Stage 49.4 requires two complete profiles for comparisons", {
  data = data.frame(
    Y = c(2, 4, 5, 8, 10, 11),
    X = 1:6,
    Group = factor(c("A", "A", "B", "B", "A", "B"))
  )
  model = stats::lm(Y ~ X + Group, data = data)
  objective = buildResearchQuestionObjective(
    model,
    "Compare the expected response for X = 2 versus X = 5."
  )

  testthat::expect_identical(objective$archetype, "compare_groups_or_profiles")
  testthat::expect_identical(objective$answerPayload$status, "needs_input")
  testthat::expect_true("Group" %in% objective$answerPayload$missingPredictors)
})
