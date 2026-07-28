#' Build a deterministic research-question objective
#'
#' Converts a research question and the existing Stage 47 routing payloads into
#' a small, inspectable contract describing what a satisfactory explanation
#' should answer. This stage records the objective without changing visible
#' explanation ordering.
#'
#' @param model Fitted model object.
#' @param researchQuestion Character scalar research question.
#'
#' @return A validated `wmfmQuestionObjective` list.
#' @keywords internal
#' @noRd
buildResearchQuestionObjective = function(model, researchQuestion) {
  originalText = trimws(as.character(researchQuestion %||% ""))
  originalText = if (length(originalText)) originalText[[1]] else ""
  normalizedText = tolower(gsub("\\s+", " ", originalText, perl = TRUE))

  route = buildResearchQuestionRoute(
    model = model,
    researchQuestion = originalText
  )
  predictionPayload = buildResearchQuestionPredictionPayload(
    model = model,
    researchQuestion = originalText,
    includeQuestionRoute = FALSE
  )

  archetype = classifyResearchQuestionArchetype(
    normalizedText = normalizedText,
    route = route,
    predictionPayload = predictionPayload
  )

  answerPayload = buildResearchQuestionAnswerPayload(
    model = model,
    researchQuestion = originalText,
    archetype = archetype
  )

  predictionResult = predictionPayload$predictionResult %||% list()
  profile = predictionResult$resolvedPredictorValues %||%
    predictionResult$suppliedPredictorValues %||%
    answerPayload$leftProfile %||% list()
  missingInformation = unique(c(
    as.character(route$missingInformation %||% character(0)),
    as.character(predictionResult$missingPredictors %||% character(0)),
    as.character(answerPayload$missingPredictors %||% character(0))
  ))

  if (archetype %in% c("individual_prediction", "expected_response") &&
      !is.list(predictionPayload)) {
    requiredPredictors = names(stats::model.frame(model))[-1]
    missingInformation = unique(c(missingInformation, requiredPredictors))
  }

  concepts = researchQuestionObjectiveConcepts(
    archetype = archetype,
    model = model
  )

  objective = list(
    originalText = originalText,
    normalizedText = normalizedText,
    archetype = archetype,
    primaryObjective = researchQuestionPrimaryObjective(
      archetype = archetype,
      originalText = originalText
    ),
    profile = profile,
    essentialConcepts = concepts$essential,
    supportingConcepts = concepts$supporting,
    unsupportedOrMissing = missingInformation,
    status = route$status %||% "answerable",
    route = route$route %||% "model_answer",
    reason = route$reason %||% "general_model_question",
    requiresFollowup = length(missingInformation) > 0L ||
      (route$status %||% "answerable") %in% c("needs_input", "needs_clarification"),
    predictionPayload = predictionPayload,
    answerPayload = answerPayload
  )

  class(objective) = c("wmfmQuestionObjective", "list")
  validateWmfmQuestionObjective(objective)
}

#' @keywords internal
#' @noRd
classifyResearchQuestionArchetype = function(normalizedText, route, predictionPayload = NULL) {
  if (grepl("\\b(compare|comparison|difference between|versus|vs\\.?|higher than|lower than)\\b", normalizedText, perl = TRUE)) {
    return("compare_groups_or_profiles")
  }

  personalOutcomePattern = paste(
    c(
      "\\bwill\\s+(i|we|the student|this student|a student|the patient|this patient)\\b",
      "\\bwhat\\s+(?:mark|score|result|outcome|count)\\s+would\\s+(i|we|the student|this student|a student|the patient|this patient)\\b",
      "\\bchance\\s+(?:that\\s+)?(i|we|the student|this student|a student|the patient|this patient)\\b"
    ),
    collapse = "|"
  )

  if (is.list(predictionPayload)) {
    predictionType = predictionPayload$predictionResult$predictionType %||% ""
    if (identical(predictionType, "individual_prediction_interval") ||
        grepl("\\b(i|me|my|individual|person|student|patient)\\b", normalizedText, perl = TRUE)) {
      return("individual_prediction")
    }
    return("expected_response")
  }

  if (grepl(personalOutcomePattern, normalizedText, perl = TRUE)) {
    return("individual_prediction")
  }

  if ((route$route %||% "") %in% c(
    "needs_input",
    "needs_clarification",
    "alternative_analysis_needed",
    "out_of_scope"
  )) {
    return("assess_model_capability")
  }

  if (grepl("\\b(uncertainty|confidence interval|prediction interval|standard error|margin of error)\\b", normalizedText, perl = TRUE)) {
    return("explain_uncertainty")
  }

  if (grepl("\\b(compare|comparison|difference between|versus|vs\\.?|higher than|lower than)\\b", normalizedText, perl = TRUE)) {
    return("compare_groups_or_profiles")
  }

  if (grepl("\\b(effect|association|relationship|change in|increase in|decrease in|impact)\\b", normalizedText, perl = TRUE)) {
    return("estimate_or_interpret_effect")
  }

  if (grepl("\\b(expected|average|mean|probability|expected count|expected response)\\b", normalizedText, perl = TRUE)) {
    return("expected_response")
  }

  "explain_fitted_model"
}

#' @keywords internal
#' @noRd
researchQuestionPrimaryObjective = function(archetype, originalText) {
  labels = c(
    explain_fitted_model = "Explain the principal fitted relationships and their uncertainty.",
    estimate_or_interpret_effect = "Estimate or interpret the predictor effect requested in the research question.",
    compare_groups_or_profiles = "Compare the groups or predictor profiles named in the research question.",
    individual_prediction = "Predict an individual outcome for the supplied predictor profile and explain its uncertainty.",
    expected_response = "Estimate the expected or average response for the requested conditions.",
    explain_uncertainty = "Explain the requested uncertainty quantity and what it does and does not describe.",
    assess_model_capability = "Assess whether the fitted model and supplied information can answer the research question."
  )

  objective = unname(labels[[archetype]])
  if (is.null(objective)) {
    objective = "Answer the research question using the fitted model."
  }

  if (!nzchar(originalText)) {
    return(objective)
  }

  paste0(objective, " Research question: ", originalText)
}

#' @keywords internal
#' @noRd
researchQuestionObjectiveConcepts = function(archetype, model) {
  modelFamily = if (inherits(model, "glm")) {
    family(model)$family
  } else {
    "gaussian"
  }

  concepts = switch(
    archetype,
    explain_fitted_model = list(
      essential = c("principal fitted relationships", "effect uncertainty"),
      supporting = c("model fit")
    ),
    estimate_or_interpret_effect = list(
      essential = c("direction", "magnitude", "response scale", "uncertainty"),
      supporting = c("reference group", "model fit")
    ),
    compare_groups_or_profiles = list(
      essential = c("defined comparison", "response scale", "comparison uncertainty"),
      supporting = c("reference group", "supporting fitted effects")
    ),
    individual_prediction = list(
      essential = c("individual prediction", "practical uncertainty"),
      supporting = c("supporting fitted effects", "model fit")
    ),
    expected_response = list(
      essential = c("expected response", "confidence interval"),
      supporting = c("supporting fitted effects", "model fit")
    ),
    explain_uncertainty = list(
      essential = c("interval type", "uncertainty interpretation", "limits of interpretation"),
      supporting = c("point estimate")
    ),
    assess_model_capability = list(
      essential = c("answerability", "missing information", "model limitations"),
      supporting = c("more suitable question or model")
    )
  )

  if (identical(archetype, "individual_prediction")) {
    if (identical(modelFamily, "gaussian")) {
      concepts$essential = c(concepts$essential, "prediction interval")
    } else if (identical(modelFamily, "binomial")) {
      concepts$essential = c(concepts$essential, "predicted probability")
    } else if (identical(modelFamily, "poisson")) {
      concepts$essential = c("expected count", "future count uncertainty")
    }
  }

  concepts
}

#' Validate a deterministic research-question objective
#'
#' @param objective Candidate objective object.
#'
#' @return The objective invisibly.
#' @keywords internal
#' @noRd
validateWmfmQuestionObjective = function(objective) {
  requiredNames = c(
    "originalText",
    "normalizedText",
    "archetype",
    "primaryObjective",
    "profile",
    "essentialConcepts",
    "supportingConcepts",
    "unsupportedOrMissing",
    "status",
    "route",
    "reason",
    "requiresFollowup",
    "predictionPayload",
    "answerPayload"
  )
  missingNames = setdiff(requiredNames, names(objective))
  if (length(missingNames)) {
    stop(
      "Question objective is missing required fields: ",
      paste(missingNames, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  validArchetypes = c(
    "explain_fitted_model",
    "estimate_or_interpret_effect",
    "compare_groups_or_profiles",
    "individual_prediction",
    "expected_response",
    "explain_uncertainty",
    "assess_model_capability"
  )
  if (!objective$archetype %in% validArchetypes) {
    stop("Question objective archetype is invalid.", call. = FALSE)
  }

  if (!is.logical(objective$requiresFollowup) ||
      length(objective$requiresFollowup) != 1L ||
      is.na(objective$requiresFollowup)) {
    stop("Question objective requiresFollowup must be a logical scalar.", call. = FALSE)
  }

  invisible(objective)
}
