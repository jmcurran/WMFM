#' Append deterministic follow-up prediction answer text
#'
#' Adds a deterministic follow-up answer after the language-model explanation
#' when WMFM has computed a supported prediction payload. This keeps the
#' student-visible follow-up answer tied to WMFM-computed quantities even if the
#' language model omits or under-emphasises the follow-up request.
#'
#' @param explanation Character scalar explanation returned by the chat provider.
#' @param model Fitted model object carrying the follow-up payload attribute.
#'
#' @return Character scalar explanation, with a deterministic follow-up paragraph
#'   appended when available.
#' @keywords internal
#' @noRd
appendDeterministicFollowupAnswer = function(explanation, model) {
  answer = buildDeterministicFollowupAnswer(model = model)
  if (!nzchar(answer)) {
    return(explanation)
  }

  if (grepl(answer, explanation, fixed = TRUE)) {
    return(explanation)
  }

  explanation = removeConflictingLlmFollowupPredictionText(
    explanation = explanation,
    model = model
  )
  explanation = removeDuplicateLlmObservationFollowupText(
    explanation = explanation,
    model = model
  )

  paste(trimws(as.character(explanation %||% "")), answer, sep = "\n\n")
}

#' Build deterministic follow-up prediction answer text
#'
#' @param model Fitted model object carrying the follow-up payload attribute.
#'
#' @return Character scalar answer text, or an empty string when unavailable.
#' @keywords internal
#' @noRd
buildDeterministicFollowupAnswer = function(model) {
  payload = attr(model, "wmfm_model_followup_payload", exact = TRUE)
  if (!is.list(payload)) {
    return("")
  }

  if (identical(payload$category, "adjustment_prediction_comparison")) {
    return(buildDeterministicAdjustmentComparisonAnswer(payload = payload))
  }

  if (identical(payload$category, "observation_residual_request")) {
    return(buildDeterministicObservationResidualAnswer(payload = payload))
  }

  if (identical(payload$category, "comparable_observation_request")) {
    return(buildDeterministicComparableObservationAnswer(payload = payload))
  }

  if (!(identical(payload$category, "prediction_request") || identical(payload$category, "prediction_interval_request"))) {
    return("")
  }

  prediction = payload$predictionResult
  if (!is.list(prediction) || !identical(prediction$status, "ok")) {
    return(buildDeterministicFollowupFailureAnswer(prediction = prediction))
  }

  modelResponseName = names(stats::model.frame(model))[[1]]
  responseName = prediction$originalResponseVariable %||%
    prediction$responseDescription %||%
    modelResponseName
  settingsText = formatFollowupPredictorSettings(prediction$resolvedPredictorValues)
  fittedText = formatFollowupPredictionNumber(prediction$fittedPrediction)
  predictionSentence = if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "probability")) {
    sprintf(
      "For the follow-up question, using %s, WMFM predicts a probability for %s of %s.",
      settingsText,
      modelResponseName,
      fittedText
    )
  } else if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "expected_count")) {
    sprintf(
      "For the follow-up question, using %s, WMFM predicts an expected count for %s of %s.",
      settingsText,
      modelResponseName,
      fittedText
    )
  } else if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "odds")) {
    sprintf(
      "For the follow-up question, using %s, WMFM predicts odds for %s of %s.",
      settingsText,
      modelResponseName,
      fittedText
    )
  } else {
    sprintf(
      "For the follow-up question, using %s, WMFM predicts an expected %s of %s.",
      settingsText,
      responseName,
      fittedText
    )
  }

  pieces = c(predictionSentence)

  if (is.list(prediction$predictionInterval)) {
    interval = prediction$predictionInterval
    pieces = c(
      pieces,
      sprintf(
        "For an individual with these characteristics, a 95%% prediction interval for %s is %s to %s.",
        responseName,
        formatFollowupPredictionNumber(interval$lwr),
        formatFollowupPredictionNumber(interval$upr)
      )
    )
  }

  if (is.list(prediction$confidenceInterval)) {
    interval = prediction$confidenceInterval
    confidenceSentence = if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "probability")) {
      sprintf(
        "The 95%% confidence interval for the predicted probability is %s to %s.",
        formatFollowupPredictionNumber(interval$lwr),
        formatFollowupPredictionNumber(interval$upr)
      )
    } else if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "expected_count")) {
      sprintf(
        "The 95%% confidence interval for the expected count is %s to %s.",
        formatFollowupPredictionNumber(interval$lwr),
        formatFollowupPredictionNumber(interval$upr)
      )
    } else if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "odds")) {
      sprintf(
        "The 95%% confidence interval for the predicted odds is %s to %s.",
        formatFollowupPredictionNumber(interval$lwr),
        formatFollowupPredictionNumber(interval$upr)
      )
    } else {
      sprintf(
        "The estimated average %s for these characteristics has a 95%% confidence interval from %s to %s.",
        responseName,
        formatFollowupPredictionNumber(interval$lwr),
        formatFollowupPredictionNumber(interval$upr)
      )
    }
    pieces = c(pieces, confidenceSentence)
  }

  warningsText = paste(prediction$warnings %||% character(0), collapse = " ")
  if (nzchar(trimws(warningsText))) {
    pieces = c(pieces, warningsText)
  }

  paste(pieces, collapse = " ")
}


#' Build deterministic observation-residual follow-up answer text
#'
#' @param payload Follow-up payload carrying an observation-residual result.
#'
#' @return Character scalar answer text, or an empty string when unavailable.
#' @keywords internal
#' @noRd
buildDeterministicObservationResidualAnswer = function(payload) {
  result = payload$observationResidualResult
  if (!is.list(result)) {
    return("")
  }

  if (!identical(result$status, "ok")) {
    guidance = trimws(paste(result$warnings %||% character(0), collapse = " "))
    if (!nzchar(guidance)) {
      guidance = "WMFM could not compute this residual ranking from the fitted model."
    }

    return(paste(
      "For the follow-up question, WMFM could not compute an existing-observation residual ranking.",
      guidance,
      "WMFM has not invented or estimated any ranked observations."
    ))
  }

  observations = result$observations
  if (!is.data.frame(observations) || !nrow(observations)) {
    return("")
  }

  directionText = switch(
    result$direction %||% "absolute",
    lower = "the most negative raw residuals",
    higher = "the most positive raw residuals",
    absolute = "the largest absolute raw residuals",
    "the requested raw-residual ranking"
  )

  answerLines = c(
    sprintf(
      "For the follow-up question, WMFM ranked the %s observations with %s among the %s observations used to fit this model.",
      result$observationCount,
      directionText,
      result$totalFittedObservations
    ),
    formatDeterministicObservationResidualRows(observations)
  )

  caution = paste(
    "These are comparisons with fitted values under the current model.",
    "They do not by themselves show that an observation is a bargain, anomaly, outlier, data error, or causal effect."
  )

  paste(c(answerLines, caution), collapse = "\n")
}

#' Format deterministic observation-residual answer rows
#'
#' @param observations Ranked observation data frame.
#'
#' @return Character vector with one sentence for each ranked observation.
#' @keywords internal
#' @noRd
formatDeterministicObservationResidualRows = function(observations) {
  if (!is.data.frame(observations) || !nrow(observations)) {
    return(character(0))
  }

  vapply(seq_len(nrow(observations)), function(index) {
    row = observations[index, , drop = FALSE]
    residual = as.numeric(row$residual[[1]])
    relation = if (residual < 0) {
      "below"
    } else if (residual > 0) {
      "above"
    } else {
      "equal to"
    }

    differenceText = if (identical(relation, "equal to")) {
      "with a raw residual of 0"
    } else {
      sprintf(
        "%s its fitted value by %s, giving a raw residual of %s",
        relation,
        formatFollowupPredictionNumber(abs(residual)),
        formatFollowupPredictionNumber(residual)
      )
    }

    sprintf(
      "%s. %s (source row %s) had an observed value of %s and a fitted value of %s. It was %s.",
      row$rank[[1]],
      row$observation[[1]],
      row$row[[1]],
      formatFollowupPredictionNumber(row$observed[[1]]),
      formatFollowupPredictionNumber(row$fitted[[1]]),
      differenceText
    )
  }, character(1))
}


#' Build deterministic comparable-observation follow-up answer text
#'
#' @param payload Follow-up payload carrying a comparable-observation result.
#'
#' @return Character scalar answer text, or an empty string when unavailable.
#' @keywords internal
#' @noRd
buildDeterministicComparableObservationAnswer = function(payload) {
  result = payload$comparableObservationResult
  if (!is.list(result)) {
    return("")
  }

  if (!identical(result$status, "ok")) {
    guidance = trimws(paste(result$warnings %||% character(0), collapse = " "))
    if (!nzchar(guidance)) {
      guidance = "WMFM could not identify comparable fitted observations from the supplied predictor values."
    }

    return(paste(
      "For the follow-up question, WMFM could not compute comparable observations.",
      guidance,
      "WMFM has not invented comparable cases or a bargain threshold."
    ))
  }

  observations = result$observations
  if (!is.data.frame(observations) || !nrow(observations)) {
    return("")
  }

  settingsText = formatFollowupPredictorSettings(result$resolvedPredictorValues)
  responseValues = as.numeric(observations$response)
  summarySentence = sprintf(
    paste(
      "Across these %s nearest fitted observations, the observed %s values ranged from %s to %s,",
      "with a median of %s."
    ),
    nrow(observations),
    result$responseName,
    formatFollowupPredictionNumber(min(responseValues, na.rm = TRUE)),
    formatFollowupPredictionNumber(max(responseValues, na.rm = TRUE)),
    formatFollowupPredictionNumber(stats::median(responseValues, na.rm = TRUE))
  )

  displayCount = min(5L, nrow(observations))
  displayed = observations[seq_len(displayCount), , drop = FALSE]
  answerLines = c(
    sprintf(
      "For the follow-up question, using %s, WMFM found the %s nearest observations among the %s observations used to fit this model.",
      settingsText,
      result$neighbourCount,
      result$totalFittedObservations
    ),
    summarySentence,
    formatDeterministicComparableObservationRows(displayed)
  )

  if (nrow(observations) > displayCount) {
    answerLines = c(
      answerLines,
      sprintf(
        "Only the first %s comparable observations are listed here; The summary uses all %s selected observations.",
        displayCount,
        nrow(observations)
      )
    )
  }

  caution = paste(
    "Similarity is based only on predictors in the fitted model.",
    "These comparisons do not by themselves establish that a case is a bargain, unusually good value, or causally different.",
    "To assess whether a particular case is good value, compare its asking price with these observed prices."
  )

  paste(c(answerLines, caution), collapse = "\n")
}

#' Format deterministic comparable-observation answer rows
#'
#' @param observations Comparable-observation data frame.
#'
#' @return Character vector with one sentence for each comparable observation.
#' @keywords internal
#' @noRd
formatDeterministicComparableObservationRows = function(observations) {
  if (!is.data.frame(observations) || !nrow(observations)) {
    return(character(0))
  }

  vapply(seq_len(nrow(observations)), function(index) {
    row = observations[index, , drop = FALSE]
    sprintf(
      "%s. %s (source row %s) had an observed response of %s and a similarity distance of %s.",
      row$rank[[1]],
      row$observation[[1]],
      row$row[[1]],
      formatFollowupPredictionNumber(row$response[[1]]),
      formatFollowupPredictionNumber(row$distance[[1]])
    )
  }, character(1))
}

#' Build deterministic adjustment-comparison follow-up answer text
#'
#' @param payload Follow-up payload carrying an adjustment-comparison result.
#'
#' @return Character scalar answer text, or an empty string when unavailable.
#' @keywords internal
#' @noRd
buildDeterministicAdjustmentComparisonAnswer = function(payload) {
  comparison = payload$adjustmentComparisonResult
  if (!is.list(comparison) || !identical(comparison$status, "ok")) {
    return("")
  }

  conclusion = trimws(as.character(comparison$studentFacingConclusion %||% ""))
  caution = trimws(as.character(comparison$studentFacingCaution %||% ""))

  if (!nzchar(conclusion)) {
    return("")
  }

  if (!nzchar(caution)) {
    return(conclusion)
  }

  paste(conclusion, caution)
}


#' Remove duplicated language-model observation follow-up text
#'
#' @param explanation Character scalar returned by the chat provider.
#' @param model Fitted model object carrying the follow-up payload attribute.
#'
#' @return Character scalar with duplicated observation-answer paragraphs removed.
#' @keywords internal
#' @noRd
removeDuplicateLlmObservationFollowupText = function(explanation, model) {
  text = trimws(as.character(explanation %||% ""))
  if (!nzchar(text)) {
    return(text)
  }

  payload = attr(model, "wmfm_model_followup_payload", exact = TRUE)
  if (!is.list(payload) || !identical(payload$category, "observation_residual_request")) {
    return(text)
  }

  result = payload$observationResidualResult
  if (!is.list(result) || !identical(result$status, "ok")) {
    return(text)
  }

  paragraphs = strsplit(text, "\\n\\s*\\n", perl = TRUE)[[1]]
  keep = !vapply(paragraphs, isDuplicateLlmObservationResidualParagraph, logical(1))
  trimws(paste(trimws(paragraphs[keep]), collapse = "\n\n"))
}

#' Detect a duplicated language-model residual-ranking paragraph
#'
#' @param paragraph Character scalar paragraph candidate.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
isDuplicateLlmObservationResidualParagraph = function(paragraph) {
  text = tolower(trimws(as.character(paragraph %||% "")))
  if (!nzchar(text)) {
    return(FALSE)
  }

  hasRankingCue = grepl("residual", text, fixed = TRUE) ||
    grepl("fitted value", text, fixed = TRUE) ||
    grepl("expected to score", text, fixed = TRUE) ||
    grepl("performed better than expected", text, fixed = TRUE) ||
    grepl("performed worse than expected", text, fixed = TRUE) ||
    grepl("most unusual", text, fixed = TRUE) ||
    grepl("furthest from", text, fixed = TRUE)
  observationMentions = regmatches(
    text,
    gregexpr("\\b(?:row|student) [0-9]+", text, perl = TRUE)
  )[[1]]
  hasObservationCue = length(observationMentions) >= 1L ||
    grepl("observed", text, fixed = TRUE)
  hasRankedListCue = length(observationMentions) >= 2L ||
    grepl("\\b(?:five|5|three|3|two|2) (?:rows|students|observations)", text, perl = TRUE)
  hasMultipleValues = length(regmatches(text, gregexpr("[0-9]+(?:\\.[0-9]+)?", text, perl = TRUE))[[1]]) >= 3L

  isTRUE(hasRankingCue && hasObservationCue && (hasRankedListCue || hasMultipleValues))
}

#' Remove conflicting language-model follow-up prediction text
#'
#' @param explanation Character scalar returned by the chat provider.
#' @param model Fitted model object carrying the follow-up payload attribute.
#'
#' @return Character scalar with obvious conflicting follow-up prediction
#'   fallback paragraphs removed before WMFM appends its deterministic answer.
#' @keywords internal
#' @noRd
removeConflictingLlmFollowupPredictionText = function(explanation, model) {
  text = trimws(as.character(explanation %||% ""))
  if (!nzchar(text)) {
    return(text)
  }

  payload = attr(model, "wmfm_model_followup_payload", exact = TRUE)
  if (!is.list(payload)) {
    return(text)
  }

  prediction = payload$predictionResult
  if (!is.list(prediction)) {
    return(text)
  }

  if (!(identical(payload$category, "prediction_request") || identical(payload$category, "prediction_interval_request"))) {
    return(text)
  }

  paragraphs = strsplit(text, "\\n\\s*\\n", perl = TRUE)[[1]]
  cleanedParagraphs = vapply(paragraphs, removeLlmFollowupPredictionSentences, character(1))
  cleanedParagraphs = trimws(cleanedParagraphs)
  cleanedParagraphs = cleanedParagraphs[nzchar(cleanedParagraphs)]

  trimws(paste(cleanedParagraphs, collapse = "\n\n"))
}

#' Remove language-model follow-up prediction sentences
#'
#' @param paragraph Character scalar paragraph candidate.
#'
#' @return Character scalar with LLM-authored follow-up prediction sentences removed.
#' @keywords internal
#' @noRd
removeLlmFollowupPredictionSentences = function(paragraph) {
  text = trimws(as.character(paragraph %||% ""))
  if (!nzchar(text)) {
    return("")
  }

  if (isConflictingLlmFollowupPredictionParagraph(text)) {
    return("")
  }

  sentences = strsplit(text, "(?<=[.!?])\\s+", perl = TRUE)[[1]]
  keep = vapply(sentences, function(sentence) {
    !isLlmFollowupPredictionSentence(sentence)
  }, logical(1))

  trimws(paste(sentences[keep], collapse = " "))
}

#' Detect language-model-authored follow-up prediction sentences
#'
#' @param sentence Character scalar sentence candidate.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
isLlmFollowupPredictionSentence = function(sentence) {
  text = tolower(trimws(as.character(sentence %||% "")))
  if (!nzchar(text)) {
    return(FALSE)
  }

  hasPredictionCue = grepl("\\bpredicted?\\b", text, perl = TRUE) ||
    grepl("\\bexpected\\b", text, perl = TRUE) ||
    grepl("prediction interval", text, fixed = TRUE)

  hasFollowupCue = grepl("follow-up", text, fixed = TRUE) ||
    grepl("follow up", text, fixed = TRUE) ||
    grepl("\\bif\\b", text, perl = TRUE) ||
    grepl("\\bwhen\\b", text, perl = TRUE) ||
    grepl("\\bwith\\b", text, perl = TRUE) ||
    grepl("\\busing\\b.*[A-Za-z][A-Za-z0-9_.]*\\s*=", text, perl = TRUE) ||
    grepl("\\bfor\\b.*[A-Za-z][A-Za-z0-9_.]*\\s*=", text, perl = TRUE)

  hasIndividualIntervalCue = grepl("individual", text, fixed = TRUE) &&
    grepl("prediction interval", text, fixed = TRUE)

  isTRUE((hasPredictionCue && hasFollowupCue) || hasIndividualIntervalCue)
}

#' Detect conflicting language-model follow-up prediction paragraphs
#'
#' @param paragraph Character scalar paragraph candidate.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
isConflictingLlmFollowupPredictionParagraph = function(paragraph) {
  text = tolower(trimws(as.character(paragraph %||% "")))
  if (!nzchar(text)) {
    return(FALSE)
  }

  hasFollowupCue = grepl("\\bfollow[- ]?up\\b", text, perl = TRUE) ||
    grepl("\\bto predict\\b", text, perl = TRUE) ||
    grepl("\\bpredicted?\\b", text, perl = TRUE)

  hasUnsafeFallback = grepl("all other predictors", text, fixed = TRUE) ||
    grepl("original data set", text, fixed = TRUE) ||
    grepl("dataset", text, fixed = TRUE) && grepl("need|values|required", text, perl = TRUE) ||
    grepl("cannot be made", text, fixed = TRUE) ||
    grepl("cannot make", text, fixed = TRUE) ||
    grepl("could not compute", text, fixed = TRUE)

  isTRUE(hasFollowupCue && hasUnsafeFallback)
}

#' Build deterministic follow-up failure answer text
#'
#' @param prediction Prediction result list.
#'
#' @return Character scalar answer text, or empty string when no failure should
#'   be displayed.
#' @keywords internal
#' @noRd
buildDeterministicFollowupFailureAnswer = function(prediction) {
  if (!is.list(prediction)) {
    return("")
  }

  status = as.character(prediction$status %||% "")
  if (!nzchar(status) || identical(status, "ok")) {
    return("")
  }

  warningText = paste(prediction$warnings %||% character(0), collapse = " ")
  if (!nzchar(trimws(warningText))) {
    warningText = "WMFM could not compute this follow-up prediction from the values supplied."
  }

  if (identical(status, "extrapolation_blocked")) {
    return(paste(
      "For the follow-up question, WMFM did not compute a prediction because the requested predictor value requires unsupported extrapolation.",
      warningText
    ))
  }

  paste(
    "For the follow-up question, WMFM could not compute a deterministic prediction.",
    warningText,
    "Only predictors used by the fitted model are required; unrelated variables from the original data set are not required."
  )
}

#' Format follow-up predictor settings
#'
#' @param values Named list of resolved predictor values.
#'
#' @return Character scalar settings description.
#' @keywords internal
#' @noRd
formatFollowupPredictorSettings = function(values) {
  if (!is.list(values) || length(values) == 0) {
    return("the supplied predictor values")
  }

  valueNames = names(values)
  valueText = vapply(values, function(value) {
    if (is.numeric(value)) {
      return(formatFollowupPredictionNumber(value))
    }
    as.character(value %||% "")
  }, character(1))

  paste(paste0(valueNames, " = ", valueText), collapse = ", ")
}

#' Format deterministic follow-up prediction numbers
#'
#' @param value Numeric scalar.
#'
#' @return Character scalar formatted number.
#' @keywords internal
#' @noRd
formatFollowupPredictionNumber = function(value) {
  numericValue = suppressWarnings(as.numeric(value))
  if (!is.finite(numericValue)) {
    return(as.character(value %||% ""))
  }

  formatted = format(signif(numericValue, 4), trim = TRUE, scientific = FALSE)
  sub("\\.0+$", "", formatted, perl = TRUE)
}
