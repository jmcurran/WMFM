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

  if (identical(source, "followup_question")) {
    existingPayload = classifyModelFollowupQuestion(originalText)
    return(routeExistingModelQuestionPayload(existingPayload, source = source))
  }

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
  if (!is.null(predictionResult) && identical(predictionResult$status, "needs_input")) {
    route = "needs_input"
    status = "needs_input"
    supported = FALSE
    reason = predictionResult$reason %||% "missing_predictor_values"

    missingInformation = predictionResult$missingPredictors %||% character(0)
    if (length(missingInformation) == 0 &&
        identical(reason, "missing_predictor_values")) {
      requiredPredictors = predictionResult$requiredPredictors %||% character(0)
      suppliedPredictors = names(
        predictionResult$suppliedPredictorValues %||% list()
      )
      missingInformation = setdiff(requiredPredictors, suppliedPredictors)
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
