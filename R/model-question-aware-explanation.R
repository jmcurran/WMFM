#' Build a question-aware prompt contract
#'
#' @param model Fitted model object.
#'
#' @return Character scalar prompt block.
#' @keywords internal
#' @noRd
buildQuestionAwareExplanationPromptBlock = function(model) {
  objective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  if (!inherits(objective, "wmfmQuestionObjective")) {
    return("")
  }

  if (identical(objective$archetype, "individual_prediction")) {
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

    intervalText = if (is.list(prediction$predictionInterval)) {
      paste0(formatFollowupPredictionNumber(prediction$predictionInterval$lwr), " to ", formatFollowupPredictionNumber(prediction$predictionInterval$upr))
    } else {
      "not supplied"
    }
    return(paste(
      "Question-aware explanation contract:",
      "Primary task: answer the student's individual-prediction research question directly.",
      "WMFM will place its deterministic numerical answer before the language-model explanation.",
      "A general model summary by itself does not answer this individual-prediction question.",
      "Begin with practical uncertainty; keep coefficient interpretation concise and secondary.",
      paste0("Resolved predictor profile: ", formatFollowupPredictorSettings(prediction$resolvedPredictorValues)),
      paste0("Deterministic fitted prediction: ", formatFollowupPredictionNumber(prediction$fittedPrediction)),
      paste0("Deterministic 95% individual prediction interval: ", intervalText),
      "Do not recompute, replace, contradict, or relabel these quantities.",
      sep = "\n"
    ))
  }

  if (identical(objective$archetype, "expected_response")) {
    prediction = objective$answerPayload$predictionResult %||% list()
    if (!identical(prediction$status, "ok")) {
      return(paste(
        "Question-aware explanation contract:",
        "Primary task: explain what information is missing for the expected-response estimate.",
        "Do not substitute an average profile.",
        sep = "\n"
      ))
    }
    return(paste(
      "Question-aware explanation contract:",
      "Primary task: answer the expected or average response question directly.",
      paste0("Resolved predictor profile: ", formatFollowupPredictorSettings(prediction$resolvedPredictorValues)),
      paste0("Deterministic expected response: ", formatFollowupPredictionNumber(prediction$fittedPrediction)),
      "Use the supplied confidence interval for the mean response when available.",
      "Do not call this an individual prediction and do not describe its confidence interval as a prediction interval.",
      "Keep general coefficient interpretation secondary.",
      sep = "\n"
    ))
  }

  if (identical(objective$archetype, "compare_groups_or_profiles")) {
    comparison = objective$answerPayload %||% list()
    if (!identical(comparison$status, "ok")) {
      return(paste(
        "Question-aware explanation contract:",
        "Primary task: explain that two complete comparison profiles are required.",
        "Do not invent omitted predictor values.",
        sep = "\n"
      ))
    }
    return(paste(
      "Question-aware explanation contract:",
      "Primary task: compare the two expected responses directly.",
      paste0("First profile: ", formatFollowupPredictorSettings(comparison$leftProfile)),
      paste0("Second profile: ", formatFollowupPredictorSettings(comparison$rightProfile)),
      paste0("Deterministic second-minus-first difference: ", formatFollowupPredictionNumber(comparison$difference)),
      "Interpret the supplied confidence interval as uncertainty in the expected-response difference.",
      "Do not relabel it as an individual prediction interval.",
      sep = "\n"
    ))
  }

  ""
}

#' Prepend a deterministic research-question answer
#'
#' @param explanation Character scalar language-model explanation.
#' @param model Fitted model object.
#'
#' @return Character scalar with the verified research-question answer first.
#' @keywords internal
#' @noRd
prependDeterministicResearchQuestionAnswer = function(explanation, model) {
  objective = attr(model, "wmfm_research_question_objective", exact = TRUE)
  if (!inherits(objective, "wmfmQuestionObjective")) {
    return(explanation)
  }

  answer = ""
  if (objective$archetype %in% c("individual_prediction", "expected_response")) {
    payload = if (identical(objective$archetype, "expected_response")) objective$answerPayload else objective$predictionPayload
    prediction = payload$predictionResult %||% list()
    if (is.list(payload) && identical(prediction$status, "ok")) {
      modelCopy = model
      attr(modelCopy, "wmfm_model_followup_payload") = payload
      answer = buildDeterministicFollowupAnswer(model = modelCopy)
    }
  } else if (identical(objective$archetype, "compare_groups_or_profiles")) {
    answer = buildDeterministicResearchQuestionComparisonAnswer(objective$answerPayload, model)
  }

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
