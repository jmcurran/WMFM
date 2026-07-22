testthat::test_that("shared question route validates its stable contract", {
  route = newWmfmQuestionRoute(
    originalText = "What is predicted when x = 2?",
    normalizedText = "what is predicted when x = 2?",
    source = "followup_question",
    route = "model_answer",
    status = "answerable",
    supported = TRUE,
    requiresModel = TRUE,
    requiresDeterministicComputation = TRUE,
    reason = "prediction_request"
  )

  testthat::expect_s3_class(route, "wmfmQuestionRoute")
  testthat::expect_identical(
    names(route),
    c(
      "originalText",
      "normalizedText",
      "source",
      "route",
      "status",
      "supported",
      "requiresModel",
      "requiresDeterministicComputation",
      "reason",
      "missingInformation",
      "recommendedCapability",
      "deterministicResponse",
      "existingPayload"
    )
  )
  testthat::expect_invisible(validateWmfmQuestionRoute(route))
})

testthat::test_that("route validator rejects invalid source, route, and status", {
  baseArguments = list(
    originalText = "Question",
    normalizedText = "question",
    source = "followup_question",
    route = "model_answer",
    status = "answerable",
    supported = TRUE,
    requiresModel = TRUE,
    requiresDeterministicComputation = FALSE,
    reason = "ordinary_question"
  )

  invalidSource = baseArguments
  invalidSource$source = "other"
  testthat::expect_error(
    do.call(newWmfmQuestionRoute, invalidSource),
    "source is invalid"
  )

  invalidRoute = baseArguments
  invalidRoute$route = "invented_route"
  testthat::expect_error(
    do.call(newWmfmQuestionRoute, invalidRoute),
    "route is invalid"
  )

  invalidStatus = baseArguments
  invalidStatus$status = "invented_status"
  testthat::expect_error(
    do.call(newWmfmQuestionRoute, invalidStatus),
    "status is invalid"
  )
})

testthat::test_that("existing supported follow-ups are wrapped without reclassification", {
  cases = data.frame(
    question = c(
      "What is the predicted mark for test = 10 and attendance = yes?",
      "Explain this for a 10-unit increase in Test",
      "Explain this for a beginner",
      "What is a good deal for a one carat diamond?",
      "What is the 90th percentile price for a one-carat diamond?"
    ),
    currentCategory = c(
      "prediction_request",
      "unit_change_request",
      "beginner_friendly",
      "comparable_observation_request",
      "conditional_quantile_request"
    ),
    expectedRoute = c(
      "model_answer",
      "model_answer",
      "explanation_preference",
      "model_answer",
      "alternative_analysis_needed"
    ),
    stringsAsFactors = FALSE
  )

  for (caseIndex in seq_len(nrow(cases))) {
    route = routeModelQuestion(
      question = cases$question[[caseIndex]],
      source = "followup_question"
    )

    testthat::expect_identical(
      route$existingPayload$category,
      cases$currentCategory[[caseIndex]],
      info = cases$question[[caseIndex]]
    )
    testthat::expect_identical(
      route$route,
      cases$expectedRoute[[caseIndex]],
      info = cases$question[[caseIndex]]
    )
  }
})

testthat::test_that("prediction-shaped research questions use the shared route contract", {
  data = data.frame(
    exam = c(55, 68, 74, 82, 91),
    attend = c(50, 60, 70, 80, 90),
    test = c(45, 58, 67, 76, 88)
  )
  model = stats::lm(exam ~ attend + test, data = data)

  answerableRoute = routeModelQuestion(
    question = "What exam mark is predicted when attend = 80 and test = 75?",
    source = "research_question",
    model = model
  )
  testthat::expect_identical(answerableRoute$route, "model_answer")
  testthat::expect_identical(answerableRoute$status, "answerable")
  testthat::expect_identical(
    answerableRoute$existingPayload$category,
    "prediction_interval_request"
  )

  needsInputRoute = routeModelQuestion(
    question = "What exam mark is predicted for this student?",
    source = "research_question",
    model = model
  )
  testthat::expect_identical(needsInputRoute$route, "needs_input")
  testthat::expect_identical(needsInputRoute$status, "needs_input")
  testthat::expect_identical(needsInputRoute$reason, "missing_predictor_values")
  testthat::expect_setequal(needsInputRoute$missingInformation, c("attend", "test"))
})

testthat::test_that("research prediction payload retains compatibility and attaches its route", {
  data = data.frame(
    y = c(2, 4, 6, 8, 10),
    x = c(1, 2, 3, 4, 5)
  )
  model = stats::lm(y ~ x, data = data)

  payload = buildResearchQuestionPredictionPayload(
    model = model,
    researchQuestion = "What is predicted when x = 3?"
  )

  testthat::expect_identical(payload$category, "prediction_request")
  testthat::expect_true(payload$supported)
  testthat::expect_true(payload$requiresDeterministicComputation)
  testthat::expect_s3_class(payload$questionRoute, "wmfmQuestionRoute")
  testthat::expect_identical(payload$questionRoute$route, "model_answer")
})

testthat::test_that("unusual-question corpus records current and proposed routing", {
  corpus = data.frame(
    source = c(
      "research_question",
      "research_question",
      "followup_question",
      "followup_question",
      "followup_question",
      "followup_question",
      "followup_question",
      "followup_question",
      "followup_question",
      "followup_question",
      "followup_question"
    ),
    question = c(
      "Why are you predicting my exam result?",
      "Why are we doing this?",
      "I don't know.",
      "What is regression?",
      "What is logistic regression?",
      "Will I pass the course?",
      "What is the chance I will pass?",
      "Is this a good model?",
      "Tell me something interesting.",
      "Does attendance cause better exam results?",
      "What should I ask?"
    ),
    currentCategory = c(
      "ordinary_research_question",
      "ordinary_research_question",
      "unsupported_or_out_of_scope",
      "unsupported_or_out_of_scope",
      "unsupported_or_out_of_scope",
      "prediction_request",
      "unsupported_or_out_of_scope",
      "unsupported_or_out_of_scope",
      "unsupported_or_out_of_scope",
      "unsupported_or_out_of_scope",
      "unsupported_or_out_of_scope"
    ),
    proposedRoute = c(
      "analysis_purpose",
      "analysis_purpose",
      "needs_clarification",
      "statistical_education",
      "statistical_education",
      "needs_input",
      "needs_input",
      "alternative_analysis_needed",
      "needs_clarification",
      "alternative_analysis_needed",
      "needs_clarification"
    ),
    proposedReason = c(
      "prediction_purpose",
      "analysis_purpose",
      "not_a_question",
      "definition_regression",
      "definition_logistic_regression",
      "missing_outcome_threshold",
      "missing_outcome_threshold",
      "requires_diagnostic_assessment",
      "unclear_question",
      "causal_claim_not_supported",
      "capability_guidance_requested"
    ),
    responseOwnership = c(
      "deterministic_scaffold",
      "deterministic_scaffold",
      "clarification",
      "constrained_educational",
      "constrained_educational",
      "deterministic",
      "deterministic",
      "deterministic",
      "clarification",
      "deterministic",
      "clarification"
    ),
    stringsAsFactors = FALSE
  )

  requiredColumns = c(
    "source",
    "question",
    "currentCategory",
    "proposedRoute",
    "proposedReason",
    "responseOwnership"
  )
  testthat::expect_identical(names(corpus), requiredColumns)
  testthat::expect_true(all(nzchar(corpus$question)))
  testthat::expect_true(all(corpus$source %in% c("research_question", "followup_question")))

  implementedStage47Reasons = c(
    "prediction_purpose",
    "analysis_purpose",
    "not_a_question",
    "definition_regression",
    "definition_logistic_regression",
    "unclear_question",
    "capability_guidance_requested",
    "missing_outcome_threshold",
    "requires_diagnostic_assessment",
    "causal_claim_not_supported",
    "conditional_distribution_required",
    "response_question_mismatch"
  )

  for (caseIndex in seq_len(nrow(corpus))) {
    route = routeModelQuestion(
      question = corpus$question[[caseIndex]],
      source = corpus$source[[caseIndex]]
    )

    if (corpus$proposedReason[[caseIndex]] %in% implementedStage47Reasons) {
      testthat::expect_identical(
        route$route,
        corpus$proposedRoute[[caseIndex]],
        info = corpus$question[[caseIndex]]
      )
      testthat::expect_identical(
        route$reason,
        corpus$proposedReason[[caseIndex]],
        info = corpus$question[[caseIndex]]
      )
    } else {
      observedCategory = if (identical(corpus$source[[caseIndex]], "research_question")) {
        route$reason
      } else {
        route$existingPayload$category
      }
      testthat::expect_identical(
        observedCategory,
        corpus$currentCategory[[caseIndex]],
        info = corpus$question[[caseIndex]]
      )
    }
  }

  testthat::expect_true(all(corpus$proposedRoute %in% c(
    "statistical_education",
    "analysis_purpose",
    "needs_input",
    "alternative_analysis_needed",
    "needs_clarification"
  )))
})

testthat::test_that("Stage 47.3 routes unclear, purpose, and educational questions", {
  cases = data.frame(
    source = c(
      "followup_question",
      "followup_question",
      "research_question",
      "research_question",
      "followup_question",
      "followup_question"
    ),
    question = c(
      "I don't know.",
      "Tell me something interesting.",
      "Why are you predicting my exam result?",
      "Why are we doing this?",
      "What is regression?",
      "What is logistic regression?"
    ),
    route = c(
      "needs_clarification",
      "needs_clarification",
      "analysis_purpose",
      "analysis_purpose",
      "statistical_education",
      "statistical_education"
    ),
    reason = c(
      "not_a_question",
      "unclear_question",
      "prediction_purpose",
      "analysis_purpose",
      "definition_regression",
      "definition_logistic_regression"
    ),
    stringsAsFactors = FALSE
  )

  for (caseIndex in seq_len(nrow(cases))) {
    route = routeModelQuestion(
      question = cases$question[[caseIndex]],
      source = cases$source[[caseIndex]]
    )

    testthat::expect_identical(
      route$route,
      cases$route[[caseIndex]],
      info = cases$question[[caseIndex]]
    )
    testthat::expect_identical(
      route$reason,
      cases$reason[[caseIndex]],
      info = cases$question[[caseIndex]]
    )
    testthat::expect_true(
      nzchar(route$deterministicResponse),
      info = cases$question[[caseIndex]]
    )
  }
})

testthat::test_that("Stage 47.3 follow-up routes retain a compatible payload", {
  payload = classifyAndRouteModelFollowupQuestion("What is regression?")

  testthat::expect_identical(payload$category, "question_route_response")
  testthat::expect_false(payload$supported)
  testthat::expect_s3_class(payload$questionRoute, "wmfmQuestionRoute")
  testthat::expect_identical(payload$questionRoute$route, "statistical_education")
  testthat::expect_match(payload$deterministicResponse, "Linear regression")
})

testthat::test_that("Stage 47.3 deterministic follow-up response is student visible", {
  data = data.frame(y = c(1, 2, 3, 4), x = c(1, 2, 3, 4))
  model = stats::lm(y ~ x, data = data)
  payload = classifyAndRouteModelFollowupQuestion("I don't know.")
  attr(model, "wmfm_model_followup_payload") = payload

  answer = buildDeterministicFollowupAnswer(model)

  testthat::expect_match(answer, "does not yet give WMFM a research question")
})

testthat::test_that("Stage 47.3 research routes bypass conventional model explanation", {
  data = data.frame(y = c(1, 2, 3, 4), x = c(1, 2, 3, 4))
  model = stats::lm(y ~ x, data = data)
  attr(model, "wmfm_research_question") = "What is regression?"
  attr(model, "wmfm_research_question_route") = buildResearchQuestionRoute(
    model = model,
    researchQuestion = "What is regression?"
  )

  chatCalled = FALSE
  chat = list(
    chat = function(prompt) {
      chatCalled <<- TRUE
      "This should not be returned."
    }
  )

  explanation = lmExplanation(model = model, chat = chat, useCache = FALSE)

  testthat::expect_false(chatCalled)
  testthat::expect_match(explanation, "Linear regression")
  testthat::expect_false(grepl("This should not be returned", explanation, fixed = TRUE))
})

testthat::test_that("Stage 47.3 leaves mature statistical follow-ups unchanged", {
  payload = classifyAndRouteModelFollowupQuestion(
    "What is the predicted mark for test = 10 and attendance = yes?"
  )

  testthat::expect_identical(payload$category, "prediction_request")
  testthat::expect_true(payload$supported)
  testthat::expect_s3_class(payload$questionRoute, "wmfmQuestionRoute")
  testthat::expect_identical(payload$questionRoute$route, "model_answer")
})

testthat::test_that("Stage 47.4 asks for exact missing fitted-model predictors", {
  data = data.frame(
    exam = c(55, 68, 74, 82, 91),
    attend = c(50, 60, 70, 80, 90),
    test = c(45, 58, 67, 76, 88)
  )
  model = stats::lm(exam ~ attend + test, data = data)

  payload = classifyModelFollowupQuestion(
    "What exam mark is predicted when attend = 80?"
  )
  payload = attachQuestionRouteToModelFollowupPayload(
    followupQuestion = "What exam mark is predicted when attend = 80?",
    followupPayload = payload
  )
  payload = enrichFollowupPayloadWithLmPrediction(
    model = model,
    followupPayload = payload
  )

  testthat::expect_identical(payload$questionRoute$route, "needs_input")
  testthat::expect_identical(payload$questionRoute$missingInformation, "test")

  payload = attachQuestionRouteToModelFollowupPayload(
    followupQuestion = "What exam mark is predicted when attend = 80?",
    followupPayload = payload
  )

  testthat::expect_identical(payload$questionRoute$route, "needs_input")
  testthat::expect_identical(
    payload$questionRoute$reason,
    "missing_predictor_values"
  )
  testthat::expect_identical(payload$questionRoute$missingInformation, "test")
  testthat::expect_match(
    payload$questionRoute$deterministicResponse,
    "value for test",
    fixed = TRUE
  )
  testthat::expect_false(grepl("attend = ...", payload$questionRoute$deterministicResponse, fixed = TRUE))
  testthat::expect_true(length(payload$predictionResult$completedPredictorValues) > 0)
  testthat::expect_identical(
    names(payload$predictionResult$explicitlySuppliedPredictorValues),
    "attend"
  )
})

testthat::test_that("Stage 47.4 requests an undefined outcome threshold", {
  questions = c(
    "Will I pass the course?",
    "What is the chance I will pass?",
    "Would this student fail?"
  )

  for (question in questions) {
    route = routeModelQuestion(
      question = question,
      source = "followup_question"
    )

    testthat::expect_identical(route$route, "needs_input", info = question)
    testthat::expect_identical(route$status, "needs_input", info = question)
    testthat::expect_identical(
      route$reason,
      "missing_outcome_threshold",
      info = question
    )
    testthat::expect_identical(
      route$missingInformation,
      "outcome_threshold",
      info = question
    )
    testthat::expect_match(
      route$deterministicResponse,
      "at least 50",
      fixed = TRUE,
      info = question
    )
  }
})

testthat::test_that("Stage 47.4 does not treat an explicit threshold as missing", {
  route = routeModelQuestion(
    question = "What is the chance I will pass if pass means at least 50?",
    source = "followup_question"
  )

  testthat::expect_false(identical(route$reason, "missing_outcome_threshold"))
})

testthat::test_that("deterministic follow-up answer uses the needs-input route", {
  data = data.frame(
    exam = c(55, 68, 74, 82, 91),
    attend = c(50, 60, 70, 80, 90),
    test = c(45, 58, 67, 76, 88)
  )
  model = stats::lm(exam ~ attend + test, data = data)
  question = "What exam mark is predicted when attend = 80?"

  payload = classifyModelFollowupQuestion(question)
  payload = enrichFollowupPayloadWithLmPrediction(
    model = model,
    followupPayload = payload
  )
  payload = attachQuestionRouteToModelFollowupPayload(
    followupQuestion = question,
    followupPayload = payload
  )
  attr(model, "wmfm_model_followup_payload") = payload

  answer = buildDeterministicFollowupAnswer(model)
  testthat::expect_match(answer, "value for test", fixed = TRUE)
  testthat::expect_false(grepl("could not compute a deterministic prediction", answer, fixed = TRUE))
})

testthat::test_that("Stage 47.5 routes threshold probabilities to alternative analysis", {
  questions = c(
    "What is the chance I will pass if pass means at least 50?",
    "What is the probability that the exam mark is above 50?"
  )

  for (question in questions) {
    route = routeModelQuestion(
      question = question,
      source = "followup_question"
    )

    testthat::expect_identical(
      route$route,
      "alternative_analysis_needed",
      info = question
    )
    testthat::expect_identical(
      route$reason,
      "response_question_mismatch",
      info = question
    )
    testthat::expect_match(
      route$deterministicResponse,
      "binary response model",
      fixed = TRUE,
      info = question
    )
  }
})

testthat::test_that("Stage 47.5 routes causal questions without causal claims", {
  sources = c("research_question", "followup_question")

  for (source in sources) {
    route = routeModelQuestion(
      question = "Does attendance cause better exam results?",
      source = source
    )

    testthat::expect_identical(route$route, "alternative_analysis_needed")
    testthat::expect_identical(route$reason, "causal_claim_not_supported")
    testthat::expect_match(
      route$deterministicResponse,
      "adjusted association",
      fixed = TRUE
    )
    testthat::expect_match(
      route$deterministicResponse,
      "causal analysis",
      fixed = TRUE
    )
  }
})

testthat::test_that("Stage 47.5 routes model-adequacy questions to diagnostics", {
  route = routeModelQuestion(
    question = "Is this a good model?",
    source = "followup_question"
  )

  testthat::expect_identical(route$route, "alternative_analysis_needed")
  testthat::expect_identical(route$reason, "requires_diagnostic_assessment")
  testthat::expect_identical(route$recommendedCapability, "model_diagnostics")
  testthat::expect_match(
    route$deterministicResponse,
    "residual patterns",
    fixed = TRUE
  )
})

testthat::test_that("Stage 47.5 applies conditional-distribution guidance to research questions", {
  route = routeModelQuestion(
    question = "What is a good price for a one carat diamond?",
    source = "research_question"
  )

  testthat::expect_identical(route$route, "alternative_analysis_needed")
  testthat::expect_identical(route$reason, "conditional_distribution_required")
  testthat::expect_identical(
    route$recommendedCapability,
    "conditional_quantile_model"
  )
  testthat::expect_match(
    route$deterministicResponse,
    "quantile regression",
    fixed = TRUE
  )
})

testthat::test_that("Stage 47.5 follow-up payloads expose deterministic guidance", {
  payload = classifyAndRouteModelFollowupQuestion(
    "Does attendance cause better exam results?"
  )

  testthat::expect_identical(payload$category, "question_route_response")
  testthat::expect_identical(
    payload$questionRoute$route,
    "alternative_analysis_needed"
  )
  testthat::expect_match(
    payload$deterministicResponse,
    "causal analysis",
    fixed = TRUE
  )
})

testthat::test_that("Stage 47.5.1 preserves mature follow-up classifications", {
  comparableRoute = routeModelQuestion(
    question = "What is a good deal for a one carat diamond?",
    source = "followup_question"
  )
  testthat::expect_identical(
    comparableRoute$existingPayload$category,
    "comparable_observation_request"
  )
  testthat::expect_identical(comparableRoute$route, "model_answer")

  quantileRoute = routeModelQuestion(
    question = "What is the 90th percentile price for a one-carat diamond?",
    source = "followup_question"
  )
  testthat::expect_identical(
    quantileRoute$existingPayload$category,
    "conditional_quantile_request"
  )
  testthat::expect_identical(
    quantileRoute$route,
    "alternative_analysis_needed"
  )
})


testthat::test_that("Stage 47.6 builds model-specific answerable examples", {
  courseData = data.frame(
    exam = c(58, 64, 72, 81, 88),
    attend = c(45, 55, 65, 75, 85),
    test = c(50, 60, 70, 80, 90)
  )
  model = stats::lm(exam ~ attend + test, data = courseData)

  examples = buildModelSpecificQuestionExamples(model = model)

  testthat::expect_gte(length(examples), 3)
  testthat::expect_lte(length(examples), 4)
  testthat::expect_true(any(grepl("attend", examples, fixed = TRUE)))
  testthat::expect_true(any(grepl("exam", examples, fixed = TRUE)))
  predictionExample = examples[grepl("What does the model predict", examples, fixed = TRUE)]
  testthat::expect_length(predictionExample, 1)
  testthat::expect_match(predictionExample, "attend = 65", fixed = TRUE)
  testthat::expect_match(predictionExample, "test = 70", fixed = TRUE)
})

testthat::test_that("Stage 47.6 answers capability requests from the fitted model", {
  courseData = data.frame(
    exam = c(58, 64, 72, 81, 88),
    attend = c(45, 55, 65, 75, 85),
    test = c(50, 60, 70, 80, 90)
  )
  model = stats::lm(exam ~ attend + test, data = courseData)

  route = routeModelQuestion(
    question = "What should I ask?",
    source = "research_question",
    model = model
  )

  testthat::expect_identical(route$route, "needs_clarification")
  testthat::expect_identical(
    route$recommendedCapability,
    "model_specific_question_examples"
  )
  testthat::expect_match(
    route$deterministicResponse,
    "For this fitted model",
    fixed = TRUE
  )
  testthat::expect_match(route$deterministicResponse, "attend", fixed = TRUE)
  testthat::expect_match(route$deterministicResponse, "exam", fixed = TRUE)
})

testthat::test_that("Stage 47.6 adds guidance to unclear follow-up payloads", {
  courseData = data.frame(
    exam = c(58, 64, 72, 81, 88),
    attend = c(45, 55, 65, 75, 85),
    test = c(50, 60, 70, 80, 90)
  )
  model = stats::lm(exam ~ attend + test, data = courseData)

  payload = classifyAndRouteModelFollowupQuestion(
    followupQuestion = "Tell me something interesting.",
    model = model
  )

  testthat::expect_identical(payload$category, "question_route_response")
  testthat::expect_match(
    payload$deterministicResponse,
    "For this fitted model",
    fixed = TRUE
  )
  attr(model, "wmfm_model_followup_payload") = payload
  answer = buildDeterministicFollowupAnswer(model = model)
  testthat::expect_match(answer, "attend", fixed = TRUE)
})

testthat::test_that("Stage 47.6 guides unsupported fitted-model questions", {
  courseData = data.frame(
    exam = c(58, 64, 72, 81, 88),
    attend = c(45, 55, 65, 75, 85),
    test = c(50, 60, 70, 80, 90)
  )
  model = stats::lm(exam ~ attend + test, data = courseData)

  route = routeModelQuestion(
    question = "Which restaurant should I visit?",
    source = "followup_question",
    model = model
  )

  testthat::expect_identical(route$route, "out_of_scope")
  testthat::expect_identical(
    route$recommendedCapability,
    "model_specific_question_examples"
  )
  testthat::expect_match(
    route$deterministicResponse,
    "What does the model predict for exam",
    fixed = TRUE
  )
})
