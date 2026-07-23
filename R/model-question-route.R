#' Build a shared model-question routing contract
#'
#' Creates and validates the stable named-list contract used to route research
#' and follow-up questions without changing the existing visible response
#' pathways.
#'
#' @param originalText Character scalar containing the supplied question.
#' @param normalizedText Character scalar containing normalized question text.
#' @param source One of `research_question` or `followup_question`.
#' @param route Stable high-level route name.
#' @param status One of `answerable`, `needs_input`, `unsupported`, or
#'   `needs_clarification`.
#' @param supported Logical scalar.
#' @param requiresModel Logical scalar.
#' @param requiresDeterministicComputation Logical scalar.
#' @param reason Stable reason code.
#' @param missingInformation Character vector of missing information.
#' @param recommendedCapability Optional recommended capability name.
#' @param deterministicResponse Optional deterministic response text.
#' @param existingPayload Optional existing classifier or computation payload.
#'
#' @return A validated `wmfmQuestionRoute` list.
#' @keywords internal
#' @noRd
newWmfmQuestionRoute = function(
  originalText,
  normalizedText,
  source,
  route,
  status,
  supported,
  requiresModel,
  requiresDeterministicComputation,
  reason,
  missingInformation = character(0),
  recommendedCapability = NULL,
  deterministicResponse = NULL,
  existingPayload = NULL
) {
  questionRoute = list(
    originalText = as.character(originalText %||% "")[[1]],
    normalizedText = as.character(normalizedText %||% "")[[1]],
    source = as.character(source)[[1]],
    route = as.character(route)[[1]],
    status = as.character(status)[[1]],
    supported = isTRUE(supported),
    requiresModel = isTRUE(requiresModel),
    requiresDeterministicComputation = isTRUE(requiresDeterministicComputation),
    reason = as.character(reason)[[1]],
    missingInformation = as.character(missingInformation %||% character(0)),
    recommendedCapability = recommendedCapability,
    deterministicResponse = deterministicResponse,
    existingPayload = existingPayload
  )

  class(questionRoute) = c("wmfmQuestionRoute", "list")
  validateWmfmQuestionRoute(questionRoute)
}

#' Validate a shared model-question routing contract
#'
#' @param questionRoute Candidate route object.
#'
#' @return The validated route object, invisibly.
#' @keywords internal
#' @noRd
validateWmfmQuestionRoute = function(questionRoute) {
  requiredNames = c(
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

  missingNames = setdiff(requiredNames, names(questionRoute))
  if (length(missingNames) > 0) {
    stop(
      "Question route is missing required fields: ",
      paste(missingNames, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  validSources = c("research_question", "followup_question")
  if (!questionRoute$source %in% validSources) {
    stop("Question route source is invalid.", call. = FALSE)
  }

  validRoutes = c(
    "model_answer",
    "explanation_preference",
    "statistical_education",
    "analysis_purpose",
    "needs_input",
    "alternative_analysis_needed",
    "needs_clarification",
    "out_of_scope"
  )
  if (!questionRoute$route %in% validRoutes) {
    stop("Question route is invalid.", call. = FALSE)
  }

  validStatuses = c("answerable", "needs_input", "unsupported", "needs_clarification")
  if (!questionRoute$status %in% validStatuses) {
    stop("Question route status is invalid.", call. = FALSE)
  }

  logicalFields = c("supported", "requiresModel", "requiresDeterministicComputation")
  for (logicalField in logicalFields) {
    value = questionRoute[[logicalField]]
    if (!is.logical(value) || length(value) != 1 || is.na(value)) {
      stop("Question route logical fields must be non-missing logical scalars.", call. = FALSE)
    }
  }

  if (!is.character(questionRoute$reason) || length(questionRoute$reason) != 1 ||
      !nzchar(questionRoute$reason)) {
    stop("Question route reason must be a non-empty character scalar.", call. = FALSE)
  }

  invisible(questionRoute)
}


#' Detect Stage 47.3 non-model question routes
#'
#' Recognises a deliberately small set of unclear, purpose, and educational
#' questions before the mature statistical follow-up classifier is consulted.
#'
#' @param originalText Character scalar containing the supplied question.
#' @param normalizedText Normalised lower-case question text.
#' @param source Question source.
#'
#' @return A `wmfmQuestionRoute` object when matched, otherwise `NULL`.
#' @keywords internal
#' @noRd
classifyStage47QuestionRoute = function(originalText, normalizedText, source) {
  trimmedText = trimws(normalizedText)
  punctuationFree = gsub("[[:punct:]]+$", "", trimmedText, perl = TRUE)

  nonQuestionPatterns = c(
    "i don't know",
    "i do not know",
    "idk",
    "not sure",
    "i'm not sure",
    "i am not sure",
    "no idea"
  )
  if (punctuationFree %in% nonQuestionPatterns) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = "not_a_question",
      deterministicResponse = paste(
        "That does not yet give WMFM a research question to answer.",
        "Try stating the response you want to understand and the predictor or comparison you want to investigate."
      )
    ))
  }

  if (grepl("^tell me something interesting", punctuationFree, perl = TRUE)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "unclear_question",
      deterministicResponse = paste(
        "That request is too broad for WMFM to answer safely from the fitted model.",
        "Ask about a particular predictor, comparison, prediction, or aspect of uncertainty."
      )
    ))
  }

  thresholdQuestionPattern = paste(
    c(
      "\\b(will|would|did|do|does|can)\\s+(i|we|they|the student|this student)\\s+(pass|fail|succeed)\\b",
      "\\b(chance|probability|odds)\\b.*\\b(pass|fail|success|succeed)\\b",
      "\\b(pass|fail|success)\\b.*\\b(chance|probability|odds)\\b"
    ),
    collapse = "|"
  )
  numericThresholdPattern = "\\b(?:at least|at most|above|below|over|under|greater than|less than)?\\s*[0-9]+(?:\\.[0-9]+)?\\s*%?\\b"
  if (grepl(thresholdQuestionPattern, punctuationFree, perl = TRUE) &&
      !grepl(numericThresholdPattern, punctuationFree, perl = TRUE)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_input",
      status = "needs_input",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "missing_outcome_threshold",
      missingInformation = "outcome_threshold",
      recommendedCapability = "define_outcome_threshold",
      deterministicResponse = paste(
        "WMFM needs the outcome threshold before it can interpret pass, fail, or success.",
        "State the rule explicitly, for example: pass means an exam mark of at least 50.",
        "WMFM will then determine whether the current fitted model can answer the resulting question without inventing a cutoff."
      )
    ))
  }

  explicitThresholdProbabilityPattern = paste(
    c(
      "\\b(chance|probability|odds)\\b.*\\b(pass|fail|success|succeed)\\b.*\\b(at least|at most|above|below|over|under|greater than|less than)\\b.*[0-9]",
      "\\b(pass|fail|success)\\b.*\\bmeans?\\b.*[0-9].*\\b(chance|probability|odds)\\b",
      "\\b(chance|probability|odds)\\b.*\\b(response|result|score|mark|outcome)\\b.*\\b(above|below|over|under|at least|at most)\\b.*[0-9]"
    ),
    collapse = "|"
  )
  if (grepl(explicitThresholdProbabilityPattern, punctuationFree, perl = TRUE)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "alternative_analysis_needed",
      status = "unsupported",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "response_question_mismatch",
      recommendedCapability = "binary_outcome_or_distribution_model",
      deterministicResponse = paste(
        "This asks for the probability of crossing a defined outcome threshold, not the mean outcome estimated by an ordinary continuous-response model.",
        "A direct answer requires either a binary response model for the threshold event, such as pass versus fail, or a justified model for the full outcome distribution.",
        "WMFM will not convert the current mean model into that probability automatically."
      )
    ))
  }

  if (identical(punctuationFree, "can this model answer my question")) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "question_not_specified",
      recommendedCapability = "model_specific_question_examples",
      deterministicResponse = paste(
        "That depends on the question you want the model to answer.",
        "This fitted model can describe how the mean response is associated with its predictors and can make model-based predictions when all required predictor values are supplied.",
        "State the particular question, and WMFM can say whether the current model is suitable or whether a different analysis is needed."
      )
    ))
  }

  if (identical(punctuationFree, "is this analysis useful")) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "analysis_purpose",
      status = "answerable",
      supported = TRUE,
      requiresModel = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = "analysis_usefulness",
      deterministicResponse = paste(
        "The analysis is useful only if the fitted relationship addresses the scientific or practical question you care about.",
        "It can summarise associations and uncertainty in these data, but usefulness also depends on the study design, the variables measured, and the decision the result is meant to inform."
      )
    ))
  }

  if (identical(punctuationFree, "i don't understand the question") ||
      identical(punctuationFree, "i do not understand the question")) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = "question_not_understood",
      deterministicResponse = paste(
        "That is a reasonable reason to pause before interpreting the model.",
        "Try rewriting the question by naming the response you want to understand and the predictor, group comparison, or prediction you want to investigate.",
        "WMFM should not interpret the model as an answer until the question itself is clear."
      )
    ))
  }

  if (identical(punctuationFree, "what does this result mean")) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "result_not_specified",
      deterministicResponse = paste(
        "It is not clear which result you mean.",
        "Identify the coefficient, confidence interval, fitted value, comparison, or overall conclusion you want explained, and WMFM can interpret that specific result without guessing."
      )
    ))
  }

  if (identical(punctuationFree, "should i use a different model")) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "alternative_model_requires_question_and_diagnostics",
      recommendedCapability = "model_selection_guidance",
      deterministicResponse = paste(
        "A different model may be needed, but that cannot be decided from the coefficient table alone.",
        "The choice depends on the response type, the question being asked, the study design, and model diagnostics.",
        "State what you want to learn and inspect the relevant diagnostics before replacing the current model."
      )
    ))
  }

  conditionalQuantile = classifyConditionalQuantileQuestion(punctuationFree)
  if (isTRUE(conditionalQuantile$matched)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "alternative_analysis_needed",
      status = "unsupported",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = conditionalQuantile$reasonCode,
      recommendedCapability = "conditional_quantile_model",
      deterministicResponse = conditionalQuantile$deterministicResponse
    ))
  }

  causalPattern = paste(
    c(
      "\\bcaus(?:e|es|ed|al|ality)\\b",
      "\\bprov(?:e|es|ed|ing)\\b.+\\b(improve|improves|improved|increase|increases|increased|decrease|decreases|decreased|better|worse|higher|lower)\\b",
      "\\bmake(?:s)?\\b.+\\b(better|worse|higher|lower|increase|decrease)\\b",
      "\\bif (?:i|we|they|a student|the student) (?:increase|decrease|change)\\b.+\\bwill\\b"
    ),
    collapse = "|"
  )
  if (grepl(causalPattern, punctuationFree, perl = TRUE)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "alternative_analysis_needed",
      status = "unsupported",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "causal_claim_not_supported",
      recommendedCapability = "causal_design_or_analysis",
      deterministicResponse = paste(
        "The fitted model can describe an adjusted association, but it does not by itself show that changing the predictor causes the response to change.",
        "A causal answer requires an appropriate study design and assumptions, followed by a causal analysis that addresses confounding and the direction of intervention.",
        "WMFM will not turn an observational association into a causal conclusion."
      )
    ))
  }

  diagnosticPattern = paste(
    c(
      "^is this (?:a )?good model$",
      "^is the model (?:any )?good$",
      "\\b(model|fit)\\b.*\\b(adequate|appropriate|valid|reliable|good)\\b",
      "\\b(check|test|assess|diagnose)\\b.*\\b(assumptions?|residuals?|model fit|adequacy)\\b",
      "\\bdo the residuals look (?:okay|ok|good)\\b"
    ),
    collapse = "|"
  )
  if (grepl(diagnosticPattern, punctuationFree, perl = TRUE)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "alternative_analysis_needed",
      status = "unsupported",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "requires_diagnostic_assessment",
      recommendedCapability = "model_diagnostics",
      deterministicResponse = paste(
        "An explanation of the fitted changes is not enough to decide whether the model is adequate.",
        "That judgement requires model-specific diagnostics, including residual patterns, unusual or influential observations, and checks of the assumptions relevant to the response model.",
        "WMFM has not performed that diagnostic assessment for this question, so it will not label the model good or bad from the fitted coefficients alone."
      )
    ))
  }

  if (grepl("^what should i ask", punctuationFree, perl = TRUE)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = TRUE,
      requiresDeterministicComputation = FALSE,
      reason = "capability_guidance_requested",
      deterministicResponse = paste(
        "Ask a specific question about the fitted relationship, such as whether a predictor is associated with the response,",
        "how large an estimated change is, what uncertainty remains, or what the model predicts for stated predictor values."
      )
    ))
  }

  if (grepl("^why (are you|is wmfm) predicting", punctuationFree, perl = TRUE)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "analysis_purpose",
      status = "answerable",
      supported = TRUE,
      requiresModel = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = "prediction_purpose",
      deterministicResponse = paste(
        "WMFM predicts an outcome only when the question asks what the fitted model expects for specified predictor values.",
        "A prediction summarises what the fitted model implies. It is not a judgement about the person and it does not guarantee the observed outcome."
      )
    ))
  }

  if (grepl("^why (are we|am i|is this|do we) (doing|fitting|using)", punctuationFree, perl = TRUE) ||
      identical(punctuationFree, "why are we doing this")) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "analysis_purpose",
      status = "answerable",
      supported = TRUE,
      requiresModel = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = "analysis_purpose",
      deterministicResponse = paste(
        "The fitted model is being used to describe how the response is associated with the selected predictors and to quantify uncertainty in that relationship.",
        "Whether that is useful depends on the scientific or practical question you intended to investigate."
      )
    ))
  }

  concept = NULL
  if (grepl("^what is logistic regression", punctuationFree, perl = TRUE)) {
    concept = "logistic"
  } else if (grepl("^what is poisson regression", punctuationFree, perl = TRUE)) {
    concept = "poisson"
  } else if (grepl("^what is (linear )?regression", punctuationFree, perl = TRUE)) {
    concept = "linear"
  }

  if (!is.null(concept)) {
    response = switch(
      concept,
      logistic = paste(
        "Logistic regression models a binary response by relating predictors to the log-odds of one outcome.",
        "Its fitted values are usually presented as probabilities between zero and one."
      ),
      poisson = paste(
        "Poisson regression models a non-negative count by relating predictors to the logarithm of the expected count.",
        "Exponentiated coefficients are commonly interpreted as multiplicative changes in the expected count."
      ),
      paste(
        "Linear regression models how the mean of a continuous response changes with one or more predictors.",
        "Its coefficients describe estimated changes in that mean, together with uncertainty from the fitted data."
      )
    )
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "statistical_education",
      status = "answerable",
      supported = TRUE,
      requiresModel = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = paste0("definition_", if (identical(concept, "linear")) "regression" else paste0(concept, "_regression")),
      deterministicResponse = response
    ))
  }

  NULL
}

#' Classify a follow-up question and attach its shared route
#'
#' @param followupQuestion Optional follow-up question text.
#' @param model Optional fitted model used to generate model-specific capability
#'   guidance.
#'
#' @return Existing follow-up payload with a `questionRoute` field, or a
#'   deterministic Stage 47 response payload for a newly recognised route.
#' @keywords internal
#' @noRd
classifyAndRouteModelFollowupQuestion = function(followupQuestion = NULL, model = NULL) {
  existingPayload = classifyModelFollowupQuestion(
    followupQuestion = followupQuestion
  )
  attachQuestionRouteToModelFollowupPayload(
    followupQuestion = followupQuestion,
    followupPayload = existingPayload,
    model = model
  )
}

#' Attach the shared route to an existing follow-up classification
#'
#' @param followupQuestion Optional follow-up question text.
#' @param followupPayload Existing payload returned by
#'   \code{classifyModelFollowupQuestion()}.
#' @param model Optional fitted model used to generate model-specific capability
#'   guidance.
#'
#' @return Existing follow-up payload with a `questionRoute` field, or a
#'   deterministic Stage 47 response payload for a newly recognised route.
#' @keywords internal
#' @noRd
attachQuestionRouteToModelFollowupPayload = function(
  followupQuestion = NULL,
  followupPayload,
  model = NULL
) {
  originalText = as.character(followupQuestion %||% "")
  originalText = ifelse(length(originalText) >= 1, originalText[[1]], "")
  originalText = trimws(originalText)
  normalizedText = tolower(originalText)
  normalizedText = gsub("\\s+", " ", normalizedText, perl = TRUE)

  establishedPriorityCategories = c(
    "comparable_observation_request",
    "conditional_quantile_request"
  )

  if (followupPayload$category %in% establishedPriorityCategories) {
    questionRoute = routeExistingModelQuestionPayload(
      existingPayload = followupPayload,
      source = "followup_question"
    )
  } else {
    questionRoute = classifyStage47QuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = "followup_question"
    )

    if (!inherits(questionRoute, "wmfmQuestionRoute")) {
      questionRoute = routeExistingModelQuestionPayload(
        existingPayload = followupPayload,
        source = "followup_question"
      )
    }
  }

  questionRoute = addModelSpecificCapabilityGuidance(
    questionRoute = questionRoute,
    model = model
  )

  if (is.list(questionRoute$existingPayload)) {
    payload = questionRoute$existingPayload
  } else {
    payload = list(
      originalText = questionRoute$originalText,
      normalizedText = questionRoute$normalizedText,
      category = "question_route_response",
      supported = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = questionRoute$reason,
      message = "Question handled by the shared Stage 47 route.",
      deterministicResponse = questionRoute$deterministicResponse
    )
  }
  payload$questionRoute = questionRoute
  payload
}

#' Build the route for a stored research question
#'
#' @param model Fitted model.
#' @param researchQuestion Research question text.
#'
#' @return A `wmfmQuestionRoute` object.
#' @keywords internal
#' @noRd
buildResearchQuestionRoute = function(model, researchQuestion) {
  routeModelQuestion(
    question = researchQuestion,
    source = "research_question",
    model = model,
    researchQuestion = researchQuestion
  )
}

#' Route a question through the shared Stage 47 contract
#'
#' Delegates mature follow-up requests to the existing bounded classifier and
#' wraps its result in a common route contract. Prediction-shaped research
#' questions use the existing deterministic research-prediction pathway.
#'
#' @param question Character scalar question text.
#' @param source One of `research_question` or `followup_question`.
#' @param model Optional fitted model required for deterministic research
#'   prediction routing.
#' @param researchQuestion Optional stored research question context.
#'
#' @return A validated `wmfmQuestionRoute` object.
#' @keywords internal
#' @noRd
routeModelQuestion = function(
  question,
  source = c("research_question", "followup_question"),
  model = NULL,
  researchQuestion = NULL
) {
  source = match.arg(source)
  originalText = as.character(question %||% "")
  originalText = ifelse(length(originalText) >= 1, originalText[[1]], "")
  originalText = trimws(originalText)
  normalizedText = tolower(originalText)
  normalizedText = gsub("\\s+", " ", normalizedText, perl = TRUE)

  if (!nzchar(normalizedText)) {
    return(newWmfmQuestionRoute(
      originalText = originalText,
      normalizedText = normalizedText,
      source = source,
      route = "needs_clarification",
      status = "needs_clarification",
      supported = FALSE,
      requiresModel = FALSE,
      requiresDeterministicComputation = FALSE,
      reason = "empty_research_question"
    ))
  }

  if (identical(source, "followup_question")) {
    existingPayload = classifyModelFollowupQuestion(originalText)
    establishedPriorityCategories = c(
      "comparable_observation_request",
      "conditional_quantile_request"
    )

    if (existingPayload$category %in% establishedPriorityCategories) {
      return(routeExistingModelQuestionPayload(existingPayload, source = source))
    }
  }

  stage47Route = classifyStage47QuestionRoute(
    originalText = originalText,
    normalizedText = normalizedText,
    source = source
  )
  if (inherits(stage47Route, "wmfmQuestionRoute")) {
    return(addModelSpecificCapabilityGuidance(
      questionRoute = stage47Route,
      model = model
    ))
  }

  if (identical(source, "followup_question")) {
    return(addModelSpecificCapabilityGuidance(
      questionRoute = routeExistingModelQuestionPayload(
        existingPayload,
        source = source
      ),
      model = model
    ))
  }

  if (!is.null(model) && isTRUE(isPredictionShapedResearchQuestion(originalText))) {
    existingPayload = buildResearchQuestionPredictionPayload(
      model = model,
      researchQuestion = originalText,
      includeQuestionRoute = FALSE
    )
    return(routeExistingModelQuestionPayload(existingPayload, source = source))
  }

  newWmfmQuestionRoute(
    originalText = originalText,
    normalizedText = normalizedText,
    source = source,
    route = "model_answer",
    status = "answerable",
    supported = TRUE,
    requiresModel = TRUE,
    requiresDeterministicComputation = FALSE,
    reason = "ordinary_research_question",
    existingPayload = list(researchQuestion = researchQuestion %||% originalText)
  )
}

#' Build model-specific example questions
#'
#' Generates a deliberately small set of questions that the current fitted
#' model and established WMFM pathways can answer. The examples use fitted-model
#' variable names and representative predictor values rather than generic
#' placeholders.
#'
#' @param model Optional fitted model.
#'
#' @return Character vector of example questions.
#' @keywords internal
#' @noRd
buildModelSpecificQuestionExamples = function(model) {
  if (is.null(model)) {
    return(character(0))
  }

  modelFrame = tryCatch(stats::model.frame(model), error = function(error) NULL)
  if (!is.data.frame(modelFrame) || ncol(modelFrame) < 2) {
    return(character(0))
  }

  responseName = names(modelFrame)[[1]]
  predictorNames = names(modelFrame)[-1]
  firstPredictor = predictorNames[[1]]
  examples = c(
    paste0("How is ", firstPredictor, " associated with ", responseName, "?"),
    paste0("How uncertain is the estimated relationship between ", firstPredictor, " and ", responseName, "?")
  )

  factorPredictors = predictorNames[vapply(
    modelFrame[predictorNames],
    function(value) is.factor(value) || is.character(value),
    logical(1)
  )]
  if (length(factorPredictors) > 0) {
    factorPredictor = factorPredictors[[1]]
    examples = c(
      examples,
      paste0("How does ", responseName, " differ between levels of ", factorPredictor, "?")
    )
  }

  predictorSettings = vapply(
    predictorNames,
    function(predictorName) {
      value = modelFrame[[predictorName]]
      if (is.numeric(value)) {
        representativeValue = stats::median(value, na.rm = TRUE)
        representativeText = format(
          representativeValue,
          trim = TRUE,
          scientific = FALSE,
          digits = 6
        )
      } else {
        value = as.character(value)
        value = value[!is.na(value) & nzchar(value)]
        if (length(value) == 0) {
          return(NA_character_)
        }
        valueTable = sort(table(value), decreasing = TRUE)
        representativeText = names(valueTable)[[1]]
      }
      paste0(predictorName, " = ", representativeText)
    },
    character(1)
  )
  predictorSettings = predictorSettings[!is.na(predictorSettings)]
  if (length(predictorSettings) == length(predictorNames)) {
    examples = c(
      examples,
      paste0(
        "What does the model predict for ",
        responseName,
        " when ",
        paste(predictorSettings, collapse = ", "),
        "?"
      )
    )
  }

  unique(examples)[seq_len(min(length(unique(examples)), 4))]
}

#' Add model-specific capability guidance to an unclear or unsupported route
#'
#' @param questionRoute Existing shared question route.
#' @param model Optional fitted model.
#'
#' @return Updated `wmfmQuestionRoute` object.
#' @keywords internal
#' @noRd
addModelSpecificCapabilityGuidance = function(questionRoute, model = NULL) {
  validateWmfmQuestionRoute(questionRoute)

  guidanceReasons = c(
    "not_a_question",
    "unclear_question",
    "capability_guidance_requested",
    "unsupported_or_out_of_scope"
  )
  if (!questionRoute$reason %in% guidanceReasons) {
    return(questionRoute)
  }

  examples = buildModelSpecificQuestionExamples(model = model)
  if (length(examples) == 0) {
    return(questionRoute)
  }

  numberedExamples = paste0(seq_along(examples), ". ", examples)
  baseResponse = trimws(as.character(questionRoute$deterministicResponse %||% ""))
  guidanceText = paste(
    "For this fitted model, WMFM can answer questions such as:",
    paste(numberedExamples, collapse = " ")
  )
  questionRoute$deterministicResponse = paste(
    c(baseResponse, guidanceText)[nzchar(c(baseResponse, guidanceText))],
    collapse = " "
  )
  questionRoute$recommendedCapability = "model_specific_question_examples"
  validateWmfmQuestionRoute(questionRoute)
}

#' Build a precise missing-predictor clarification
#'
#' @param missingPredictors Character vector of fitted-model predictors without
#'   supplied values.
#'
#' @return Character scalar clarification request.
#' @keywords internal
#' @noRd
buildMissingPredictorClarification = function(missingPredictors) {
  missingPredictors = unique(as.character(missingPredictors %||% character(0)))
  missingPredictors = missingPredictors[nzchar(missingPredictors)]

  if (length(missingPredictors) == 0) {
    return(paste(
      "WMFM needs values for every predictor used by the fitted model before it can calculate this prediction.",
      "Provide the values in name = value form."
    ))
  }

  predictorText = if (length(missingPredictors) == 1) {
    missingPredictors
  } else {
    paste0(
      paste(missingPredictors[-length(missingPredictors)], collapse = ", "),
      " and ",
      missingPredictors[[length(missingPredictors)]]
    )
  }

  exampleText = paste(
    paste0(missingPredictors, " = ..."),
    collapse = ", "
  )

  paste0(
    "WMFM cannot calculate this prediction until you supply ",
    if (length(missingPredictors) == 1) "a value for " else "values for ",
    predictorText,
    ". Provide only fitted-model predictors in name = value form, for example: ",
    exampleText,
    "."
  )
}

#' Wrap an existing classifier payload in the shared routing contract
#'
#' @param existingPayload Existing follow-up or research-prediction payload.
#' @param source Question source.
#'
#' @return A validated `wmfmQuestionRoute` object.
#' @keywords internal
#' @noRd
routeExistingModelQuestionPayload = function(existingPayload, source) {
  originalText = existingPayload$originalText %||% ""
  normalizedText = existingPayload$normalizedText %||%
    tolower(trimws(as.character(originalText)))
  category = existingPayload$category %||% "unsupported_or_out_of_scope"
  reason = existingPayload$reason %||% category
  supported = isTRUE(existingPayload$supported)
  requiresDeterministicComputation = isTRUE(
    existingPayload$requiresDeterministicComputation
  )

  modelAnswerCategories = c(
    "no_followup",
    "prediction_request",
    "prediction_interval_request",
    "unit_change_request",
    "proportional_change_request",
    "observation_residual_request",
    "comparable_observation_request",
    "adjustment_prediction_comparison"
  )
  explanationPreferenceCategories = c(
    "emphasis_uncertainty",
    "emphasis_effect_size",
    "emphasis_practical_interpretation",
    "emphasis_group_comparison",
    "emphasis_interaction",
    "beginner_friendly",
    "concise_answer",
    "focus_research_question"
  )

  route = "out_of_scope"
  status = "unsupported"
  requiresModel = TRUE
  missingInformation = character(0)
  deterministicResponse = existingPayload$deterministicResponse %||% NULL

  if (category %in% modelAnswerCategories) {
    route = "model_answer"
    status = "answerable"
  } else if (category %in% explanationPreferenceCategories) {
    route = "explanation_preference"
    status = "answerable"
    requiresDeterministicComputation = FALSE
  } else if (identical(category, "conditional_quantile_request")) {
    route = "alternative_analysis_needed"
    reason = existingPayload$reason %||% "requires_conditional_distribution"
  }

  predictionResult = existingPayload$predictionResult %||% NULL
  requiredPredictors = predictionResult$requiredPredictors %||% character(0)
  suppliedPredictors = names(
    predictionResult$suppliedPredictorValues %||% list()
  )
  omittedPredictors = setdiff(requiredPredictors, suppliedPredictors)
  predictionNeedsInput = !is.null(predictionResult) &&
    identical(predictionResult$status, "needs_input")
  predictionUsedCompletion = !is.null(predictionResult) &&
    identical(predictionResult$status, "ok") &&
    length(omittedPredictors) > 0

  if (isTRUE(predictionNeedsInput) || isTRUE(predictionUsedCompletion)) {
    route = "needs_input"
    status = "needs_input"
    supported = FALSE
    reason = if (isTRUE(predictionUsedCompletion)) {
      "missing_predictor_values"
    } else {
      predictionResult$reason %||% "missing_predictor_values"
    }

    missingInformation = predictionResult$missingPredictors %||% character(0)
    if (length(missingInformation) == 0 &&
        identical(reason, "missing_predictor_values")) {
      missingInformation = omittedPredictors
    }

    if (identical(reason, "missing_predictor_values")) {
      deterministicResponse = buildMissingPredictorClarification(
        missingPredictors = missingInformation
      )
    }
  }

  newWmfmQuestionRoute(
    originalText = originalText,
    normalizedText = normalizedText,
    source = source,
    route = route,
    status = status,
    supported = supported,
    requiresModel = requiresModel,
    requiresDeterministicComputation = requiresDeterministicComputation,
    reason = reason,
    missingInformation = missingInformation,
    deterministicResponse = deterministicResponse,
    existingPayload = existingPayload
  )
}
