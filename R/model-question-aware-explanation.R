#' Build a question-aware prompt contract
#'
#' @param model Fitted model object.
#'
#' @return Character scalar prompt block, or an empty string when the research
#'   question does not resolve to an individual-prediction objective.
#' @keywords internal
#' @noRd
buildQuestionAwareExplanationPromptBlock = function(model) {
  objective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  if (!inherits(objective, "wmfmQuestionObjective") ||
      !identical(objective$archetype, "individual_prediction")) {
    return("")
  }

  prediction = objective$predictionPayload$predictionResult %||% list()
  if (!identical(prediction$status, "ok")) {
    missingText = paste(objective$unsupportedOrMissing %||% character(0), collapse = ", ")
    return(paste(
      "Question-aware explanation contract:",
      "Primary task: explain that the individual prediction cannot yet be made.",
      paste0("Missing information: ", missingText),
      "Do not silently substitute average or reference predictor values.",
      sep = "\n"
    ))
  }

  profileText = formatFollowupPredictorSettings(prediction$resolvedPredictorValues)
  intervalText = if (is.list(prediction$predictionInterval)) {
    paste0(
      formatFollowupPredictionNumber(prediction$predictionInterval$lwr),
      " to ",
      formatFollowupPredictionNumber(prediction$predictionInterval$upr)
    )
  } else {
    "not supplied"
  }

  paste(
    "Question-aware explanation contract:",
    "Primary task: answer the student's individual-prediction research question directly.",
    "WMFM will place its deterministic numerical answer before the language-model explanation.",
    "Begin your contribution with a brief interpretation of that answer and its practical uncertainty.",
    "Keep coefficient interpretation concise and secondary.",
    "A general model summary by itself does not answer this research question.",
    paste0("Resolved predictor profile: ", profileText),
    paste0("Deterministic fitted prediction: ", formatFollowupPredictionNumber(prediction$fittedPrediction)),
    paste0("Deterministic 95% individual prediction interval: ", intervalText),
    "Do not recompute, replace, contradict, or relabel these quantities.",
    "Do not describe an individual prediction as a guarantee.",
    sep = "\n"
  )
}

#' Prepend a deterministic answer for an individual-prediction research question
#'
#' @param explanation Character scalar language-model explanation.
#' @param model Fitted model object.
#'
#' @return Character scalar with the verified research-question answer first.
#' @keywords internal
#' @noRd
prependDeterministicResearchQuestionAnswer = function(explanation, model) {
  objective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  if (!inherits(objective, "wmfmQuestionObjective") ||
      !identical(objective$archetype, "individual_prediction")) {
    return(explanation)
  }

  payload = objective$predictionPayload
  prediction = payload$predictionResult %||% list()
  if (!is.list(payload) || !identical(prediction$status, "ok")) {
    return(explanation)
  }

  modelCopy = model
  attr(modelCopy, "wmfm_model_followup_payload") = payload
  answer = buildDeterministicFollowupAnswer(model = modelCopy)
  if (!nzchar(answer)) {
    return(explanation)
  }

  explanationText = trimws(as.character(explanation %||% ""))
  if (grepl(answer, explanationText, fixed = TRUE)) {
    return(explanationText)
  }

  if (!nzchar(explanationText)) {
    return(answer)
  }

  paste(answer, explanationText, sep = "\n\n")
}
