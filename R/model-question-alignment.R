#' Diagnose alignment between a research question and its explanation
#'
#' Builds deterministic developer diagnostics for question-aware explanations.
#' The diagnostics assess whether the answer addresses the classified objective,
#' preserves WMFM's deterministic answer at the beginning, mentions supplied
#' profile values, and avoids common contradictions of interval meaning.
#'
#' @param explanation Character scalar explanation.
#' @param objective A `wmfmQuestionObjective` object.
#' @param mode Character scalar: `"concise"`, `"standard"`, or `"detailed"`.
#'
#' @return A list of class `wmfmQuestionAlignment`.
#' @keywords internal
#' @noRd
diagnoseQuestionAnswerAlignment = function(
    explanation,
    objective,
    mode = c("standard", "concise", "detailed")) {
  mode = match.arg(mode)
  if (!inherits(objective, "wmfmQuestionObjective")) {
    stop("`objective` must be a wmfmQuestionObjective object.", call. = FALSE)
  }

  text = trimws(paste(as.character(explanation %||% ""), collapse = " "))
  normalized = tolower(gsub("\\s+", " ", text, perl = TRUE))
  deterministic = questionObjectiveDeterministicAnswer(objective)
  deterministicFirst = questionAlignmentDeterministicFirst(
    text = text,
    objective = objective,
    deterministic = deterministic
  )

  profileValues = unlist(objective$profile %||% list(), use.names = FALSE)
  profileValues = as.character(profileValues[!is.na(profileValues)])
  profileCovered = !length(profileValues) || all(vapply(
    profileValues,
    function(value) grepl(tolower(value), normalized, fixed = TRUE),
    logical(1)
  ))

  conceptPatterns = questionAlignmentConceptPatterns()
  essential = as.character(objective$essentialConcepts %||% character(0))
  conceptCovered = vapply(essential, function(concept) {
    pattern = conceptPatterns[[concept]] %||% gsub(" ", "|", concept, fixed = TRUE)
    grepl(pattern, normalized, perl = TRUE)
  }, logical(1))

  contradictions = character(0)
  if (identical(objective$archetype, "individual_prediction")) {
    prediction = objective$predictionPayload$predictionResult %||% list()
    if (identical(prediction$responseDescription, "probability") &&
        grepl("continuous prediction interval|prediction interval from", normalized, perl = TRUE)) {
      contradictions = c(contradictions, "continuous_interval_for_binary_outcome")
    }
    if (identical(prediction$responseDescription, "expected_count") &&
        grepl("will (?:be|equal)|exact(?:ly)? [0-9]", normalized, perl = TRUE)) {
      contradictions = c(contradictions, "expected_count_presented_as_exact")
    }
  }
  if (identical(objective$archetype, "expected_response") &&
      grepl("individual prediction interval", normalized, fixed = TRUE)) {
    contradictions = c(contradictions, "confidence_interval_relabelled_as_prediction_interval")
  }

  wordCount = if (nzchar(text)) length(strsplit(text, "\\s+", perl = TRUE)[[1]]) else 0L
  modeLimit = c(concise = 180L, standard = 350L, detailed = 700L)[[mode]]
  modeAppropriate = wordCount <= modeLimit

  checks = c(
    deterministicFirst = deterministicFirst,
    profileCovered = profileCovered,
    essentialConceptsCovered = all(conceptCovered),
    noContradictions = !length(contradictions),
    modeAppropriate = modeAppropriate
  )
  score = round(100 * mean(checks))

  structure(list(
    archetype = objective$archetype,
    primaryObjective = objective$primaryObjective,
    mode = mode,
    score = score,
    checks = checks,
    essentialConceptCoverage = stats::setNames(conceptCovered, essential),
    suppliedProfile = objective$profile,
    missingInformation = objective$unsupportedOrMissing,
    contradictions = unique(contradictions),
    wordCount = wordCount,
    deterministicAnswerFirst = deterministicFirst
  ), class = c("wmfmQuestionAlignment", "list"))
}


#' @keywords internal
#' @noRd
questionAlignmentDeterministicFirst = function(text, objective, deterministic = "") {
  text = trimws(as.character(text %||% ""))
  deterministic = trimws(as.character(deterministic %||% ""))
  if (nzchar(deterministic)) {
    normalizePrefix = function(value) {
      value = tolower(trimws(as.character(value %||% "")))
      gsub("\\s+", " ", value, perl = TRUE)
    }
    if (startsWith(normalizePrefix(text), normalizePrefix(deterministic))) {
      return(TRUE)
    }
  }

  payload = if (identical(objective$archetype, "expected_response")) {
    objective$answerPayload
  } else {
    objective$predictionPayload
  }
  prediction = payload$predictionResult %||% list()
  if (!identical(prediction$status, "ok")) {
    return(TRUE)
  }

  fittedText = formatFollowupPredictionNumber(prediction$fittedPrediction)
  paragraphs = strsplit(text, "\n\\s*\n", perl = TRUE)[[1]]
  paragraphs = paragraphs[nzchar(trimws(paragraphs))]
  firstParagraph = if (length(paragraphs)) paragraphs[[1]] else ""
  normalizedFirst = tolower(gsub("\\s+", " ", trimws(firstParagraph), perl = TRUE))
  hasFittedValue = nzchar(fittedText) && grepl(fittedText, firstParagraph, fixed = TRUE)
  hasPredictionLanguage = if (identical(prediction$responseDescription, "probability")) {
    grepl("predicts? (?:a )?probability|predicted probability|fitted probability", normalizedFirst, perl = TRUE)
  } else if (identical(prediction$responseDescription, "expected_count")) {
    grepl("predicts? (?:an )?expected count|expected count", normalizedFirst, perl = TRUE)
  } else {
    grepl("predicts?|prediction|expected response", normalizedFirst, perl = TRUE)
  }

  cachedResponseUnavailable = !nzchar(deterministic)
  startsAsPredictionContinuation = grepl("^this prediction\\b", normalizedFirst, perl = TRUE)

  isTRUE(hasPredictionLanguage) &&
    (isTRUE(hasFittedValue) ||
      (isTRUE(cachedResponseUnavailable) && isTRUE(startsAsPredictionContinuation)))
}

#' @keywords internal
#' @noRd
questionObjectiveDeterministicAnswer = function(objective) {
  if (objective$archetype %in% c("individual_prediction", "expected_response")) {
    payload = if (identical(objective$archetype, "expected_response")) {
      objective$answerPayload
    } else {
      objective$predictionPayload
    }
    prediction = payload$predictionResult %||% list()
    return(trimws(as.character(prediction$deterministicResponse %||%
      payload$deterministicResponse %||% "")))
  }
  trimws(as.character(objective$answerPayload$deterministicResponse %||% ""))
}

#' @keywords internal
#' @noRd
questionAlignmentConceptPatterns = function() {
  list(
    "individual prediction" = "predict|prediction",
    "practical uncertainty" = "uncertain|interval|range|could",
    "prediction interval" = "prediction interval",
    "predicted probability" = "probability|chance",
    "expected count" = "expected count|average count|mean count",
    "future count uncertainty" = "future count|count interval|uncertain",
    "expected response" = "expected|average|mean response",
    "confidence interval" = "confidence interval",
    "defined comparison" = "compare|difference|higher|lower",
    "response scale" = "response scale|probability|expected count|mean response",
    "comparison uncertainty" = "confidence interval|uncertain|interval"
  )
}
