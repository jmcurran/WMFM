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

  explanation = removeConflictingLlmFollowupPredictionText(
    explanation = explanation,
    model = model
  )

  if (grepl(answer, explanation, fixed = TRUE)) {
    return(explanation)
  }

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

  if (!(identical(payload$category, "prediction_request") || identical(payload$category, "prediction_interval_request"))) {
    return("")
  }

  prediction = payload$predictionResult
  if (!is.list(prediction) || !identical(prediction$status, "ok")) {
    return(buildDeterministicFollowupFailureAnswer(prediction = prediction))
  }

  responseName = names(stats::model.frame(model))[[1]]
  settingsText = formatFollowupPredictorSettings(prediction$resolvedPredictorValues)
  fittedText = formatFollowupPredictionNumber(prediction$fittedPrediction)
  predictionSentence = if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "probability")) {
    sprintf(
      "For the follow-up question, using %s, WMFM predicts a probability for %s of %s.",
      settingsText,
      responseName,
      fittedText
    )
  } else if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "expected_count")) {
    sprintf(
      "For the follow-up question, using %s, WMFM predicts an expected count for %s of %s.",
      settingsText,
      responseName,
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
        "For an individual outcome with these predictor values, the 95%% prediction interval is %s to %s.",
        formatFollowupPredictionNumber(interval$lwr),
        formatFollowupPredictionNumber(interval$upr)
      )
    )
  } else if (is.list(prediction$confidenceInterval)) {
    interval = prediction$confidenceInterval
    intervalSubject = if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "probability")) {
      "predicted probability"
    } else if (identical(prediction$modelType, "glm") && identical(prediction$responseDescription, "expected_count")) {
      "expected count"
    } else {
      "average response"
    }
    pieces = c(
      pieces,
      sprintf(
        "For the %s at these predictor values, the 95%% confidence interval is %s to %s.",
        intervalSubject,
        formatFollowupPredictionNumber(interval$lwr),
        formatFollowupPredictionNumber(interval$upr)
      )
    )
  }

  warningsText = paste(prediction$warnings %||% character(0), collapse = " ")
  if (nzchar(trimws(warningsText))) {
    pieces = c(pieces, warningsText)
  }

  paste(pieces, collapse = " ")
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
  if (!is.list(prediction) || !identical(prediction$status, "ok")) {
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

  hasFollowupCue = grepl("for the follow-up question", text, fixed = TRUE) ||
    grepl("\\bif a student\\b", text, perl = TRUE) ||
    grepl("\\bif you\\b", text, perl = TRUE) ||
    grepl("\\busing attend\\s*=", text, perl = TRUE) ||
    grepl("\\busing .*test\\s*=", text, perl = TRUE)

  hasCourseCue = grepl("\\bscore", text, perl = TRUE) &&
    grepl("\\battend", text, perl = TRUE)

  hasIndividualIntervalCue = grepl("individual", text, fixed = TRUE) &&
    grepl("prediction interval", text, fixed = TRUE)

  isTRUE((hasPredictionCue && (hasFollowupCue || hasCourseCue)) || hasIndividualIntervalCue)
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
