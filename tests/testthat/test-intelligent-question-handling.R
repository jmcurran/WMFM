testthat::test_that("focused intelligent questions receive specialised routes", {
  cases = list(
    "Can this model answer my question?" = "question_not_specified",
    "Is this analysis useful?" = "analysis_usefulness",
    "I don't understand the question." = "question_not_understood",
    "What does this result mean?" = "result_not_specified",
    "Should I use a different model?" = "alternative_model_requires_question_and_diagnostics",
    "Does this prove attendance improves marks?" = "causal_claim_not_supported"
  )

  for (question in names(cases)) {
    route = routeModelQuestion(
      question = question,
      source = "followup_question"
    )

    testthat::expect_false(
      identical(route$route, "model_answer"),
      info = question
    )
    testthat::expect_identical(
      route$reason,
      cases[[question]],
      info = question
    )
    testthat::expect_true(
      nzchar(trimws(route$deterministicResponse)),
      info = question
    )
  }
})

testthat::test_that("specialised follow-up guidance replaces the ordinary explanation", {
  data = data.frame(
    exam = c(50, 60, 70, 80),
    attend = c(40, 50, 60, 70),
    test = c(5, 6, 7, 8)
  )
  model = stats::lm(exam ~ attend + test, data = data)
  payload = classifyAndRouteModelFollowupQuestion(
    "Is this analysis useful?",
    model = model
  )
  attr(model, "wmfm_model_followup_payload") = payload
  attr(model, "wmfm_model_followup_question") = "Is this analysis useful?"

  chatCalled = FALSE
  chat = list(
    chat = function(prompt) {
      chatCalled <<- TRUE
      "Ordinary full model explanation that should not appear."
    }
  )

  explanation = lmExplanation(model = model, chat = chat, useCache = FALSE)

  testthat::expect_false(chatCalled)
  testthat::expect_match(explanation, "useful only if", fixed = TRUE)
  testthat::expect_false(grepl("Ordinary full model explanation", explanation, fixed = TRUE))
})

testthat::test_that("causal proof wording is blocked explicitly", {
  route = routeModelQuestion(
    question = "Does this prove attendance improves marks?",
    source = "followup_question"
  )

  testthat::expect_identical(route$reason, "causal_claim_not_supported")
  testthat::expect_match(route$deterministicResponse, "does not by itself show", fixed = TRUE)
  testthat::expect_match(route$deterministicResponse, "causal conclusion", fixed = TRUE)
})

testthat::test_that("all focused route responses replace an existing explanation", {
  questions = c(
    "Why are you predicting my exam result?",
    "Why are we doing this?",
    "I don't know.",
    "What is the chance I will pass the course?",
    "Can this model answer my question?",
    "What should I ask instead?",
    "Is this analysis useful?",
    "What does this result mean?",
    "Does this prove attendance improves marks?",
    "I don't understand the question.",
    "Tell me whether the model is any good.",
    "Should I use a different model?"
  )

  data = data.frame(
    exam = c(50, 60, 70, 80),
    attend = c(40, 50, 60, 70),
    test = c(5, 6, 7, 8)
  )

  for (question in questions) {
    model = stats::lm(exam ~ attend + test, data = data)
    payload = classifyAndRouteModelFollowupQuestion(
      followupQuestion = question,
      model = model
    )
    attr(model, "wmfm_model_followup_payload") = payload

    answer = appendDeterministicFollowupAnswer(
      explanation = "Ordinary model summary that must be replaced.",
      model = model
    )

    testthat::expect_false(
      grepl("Ordinary model summary", answer, fixed = TRUE),
      info = question
    )
    testthat::expect_identical(
      answer,
      buildDeterministicFollowupAnswer(model = model),
      info = question
    )
  }
})

testthat::test_that("focused route response wording is grammatically clean", {
  predictionPurpose = routeModelQuestion(
    question = "Why are you predicting my exam result?",
    source = "followup_question"
  )
  modelQuality = routeModelQuestion(
    question = "Tell me whether the model is any good.",
    source = "followup_question"
  )

  testthat::expect_match(
    predictionPurpose$deterministicResponse,
    "It is not a judgement",
    fixed = TRUE
  )
  testthat::expect_false(grepl("; it is", predictionPurpose$deterministicResponse, fixed = TRUE))
  testthat::expect_match(
    modelQuality$deterministicResponse,
    "An explanation of the fitted changes",
    fixed = TRUE
  )
  testthat::expect_false(grepl("A estimated", modelQuality$deterministicResponse, fixed = TRUE))
})
