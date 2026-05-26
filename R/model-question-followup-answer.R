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

  pieces = c(
    sprintf(
      "For the follow-up question, using %s, WMFM predicts an expected %s of %s.",
      settingsText,
      responseName,
      fittedText
    )
  )

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
    pieces = c(
      pieces,
      sprintf(
        "For the average response at these predictor values, the 95%% confidence interval is %s to %s.",
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
