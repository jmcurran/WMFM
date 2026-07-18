#' Classify bounded follow-up model-question text
#'
#' Deterministically classifies bounded follow-up text into conservative
#' categories used by WMFM prompt payloads.
#'
#' @param followupQuestion Optional character scalar bounded follow-up question.
#'
#' @return Named list with fields `originalText`, `normalizedText`, `category`,
#'   `supported`, `requiresDeterministicComputation`, and `message`.
#' @keywords internal
#' @noRd
classifyModelFollowupQuestion = function(followupQuestion = NULL) {
  originalText = as.character(followupQuestion %||% "")
  originalText = ifelse(length(originalText) >= 1, originalText[[1]], "")
  normalizedText = tolower(trimws(originalText))
  normalizedText = gsub("\\s+", " ", normalizedText, perl = TRUE)

  result = list(
    originalText = trimws(originalText),
    normalizedText = normalizedText,
    category = "unsupported_or_out_of_scope",
    supported = FALSE,
    requiresDeterministicComputation = FALSE,
    message = "Request is unsupported for this pathway."
  )

  if (!nzchar(normalizedText)) {
    result$category = "no_followup"
    result$supported = TRUE
    result$message = "No follow-up question provided."
    return(result)
  }

  injectionPattern = paste(
    c(
      "ignore (all )?(previous|prior) instructions",
      "disregard (all )?(previous|prior)",
      "system prompt",
      "developer message",
      "jailbreak",
      "override",
      "bypass",
      "reveal .*prompt"
    ),
    collapse = "|"
  )

  unrelatedPattern = "\\b(poem|song|cats?|haiku|joke|story)\\b"

  if (grepl(injectionPattern, normalizedText, perl = TRUE) ||
      grepl(unrelatedPattern, normalizedText, perl = TRUE)) {
    result$category = "unsupported_or_out_of_scope"
    result$reason = "unsupported_freeform_instruction"
    result$message = "Unsupported or out-of-scope follow-up request."
    return(result)
  }

  unitChangeValues = extractRequestedUnitChangeValues(normalizedText)
  if (length(unitChangeValues) > 1L) {
    result$category = "unsupported_or_out_of_scope"
    result$supported = FALSE
    result$reason = "ambiguous_predictor_values"
    result$message = "Unsupported ambiguous multi-unit follow-up request; clarification required."
    result$unitChangeValues = unitChangeValues
    return(result)
  }

  observationResidual = classifyObservationResidualQuestion(normalizedText)
  if (isTRUE(observationResidual$matched)) {
    result$category = "observation_residual_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$reason = observationResidual$reasonCode
    result$observationDirection = observationResidual$direction
    result$message = "Existing-observation residual-ranking request captured for deterministic handling."
    return(result)
  }

  conditionalQuantile = classifyConditionalQuantileQuestion(normalizedText)
  if (isTRUE(conditionalQuantile$matched)) {
    result$category = "conditional_quantile_request"
    result$supported = FALSE
    result$requiresDeterministicComputation = FALSE
    result$reason = conditionalQuantile$reasonCode
    result$message = "Conditional-value request requires a conditional distribution rather than residual inspection of an ordinary mean model."
    result$deterministicResponse = conditionalQuantile$deterministicResponse
    return(result)
  }

  proportionalChangeValues = extractRequestedProportionalChangeValues(normalizedText)
  if (length(proportionalChangeValues) > 1L) {
    result$category = "unsupported_or_out_of_scope"
    result$supported = FALSE
    result$reason = "ambiguous_proportional_change_values"
    result$message = "Unsupported ambiguous multi-percentage follow-up request; clarification required."
    result$proportionalChangeValues = proportionalChangeValues
    return(result)
  }

  proportionalChangePattern = paste(
    c(
      "\\b(\\d+(?:\\.\\d+)?)\\s*%\\s*(increase|change|larger|higher|more)\\b",
      "\\b(increase|change)\\s+(by\\s+)?(\\d+(?:\\.\\d+)?)\\s*%\\b",
      "\\b(doubling|double|doubles|twice)\\b",
      "\\b(percentage|percent|proportional|relative)\\s+(increase|change)\\b"
    ),
    collapse = "|"
  )

  proportionalChangeIntentPattern = paste(
    c(
      "\\b(explain|interpret|describe|phrase|rephrase|express|frame)\\b",
      "\\bwhat\\s+(does|happens|is)\\b",
      "\\beffect\\b",
      "\\bslope\\b"
    ),
    collapse = "|"
  )

  if (grepl(proportionalChangePattern, normalizedText, perl = TRUE) &&
      grepl(proportionalChangeIntentPattern, normalizedText, perl = TRUE)) {
    result$category = "proportional_change_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$message = "Proportional-change interpretation request captured for deterministic log-log follow-up handling."
    result$proportionalChangeValues = proportionalChangeValues
    return(result)
  }

  adjustmentPredictionComparisonPattern = paste(
    c(
      "\\b(adjust|adjusting|adjusted|control|controlling)\\b",
      "\\b(improve|better|substantial|substantially|prediction|predictions|predictive)\\b"
    ),
    collapse = ".*"
  )

  if (grepl(adjustmentPredictionComparisonPattern, normalizedText, perl = TRUE)) {
    result$category = "adjustment_prediction_comparison"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$message = "Adjustment-comparison request captured for deterministic log-log model comparison."
    return(result)
  }

  unitChangePattern = paste(
    c(
      "\\b(\\d+(?:\\.\\d+)?)\\s*[- ]?unit\\s+(increase|change)\\b",
      "\\bfor\\s+(a\\s+)?(\\d+(?:\\.\\d+)?)\\s*[- ]?(?:[[:alpha:]][[:alnum:]_.-]*\\s+)?(increase|change)\\b",
      "\\bincrease\\s+of\\s+(\\d+(?:\\.\\d+)?)\\b",
      "\\bchange\\s+of\\s+(\\d+(?:\\.\\d+)?)\\b",
      "\\bper\\s+unit(\\s+increase|\\s+change)?\\b",
      "\\bfor\\s+a\\s+unit\\s+(increase|change)\\b",
      "\\bunit[- ]change\\b"
    ),
    collapse = "|"
  )

  unitChangeIntentPattern = paste(
    c(
      "\\b(explain|interpret|describe|phrase|rephrase|express|frame)\\b",
      "\\bwhat\\s+(does|happens|is)\\b",
      "\\beffect\\b",
      "\\bslope\\b"
    ),
    collapse = "|"
  )

  if (grepl(unitChangePattern, normalizedText, perl = TRUE) &&
      grepl(unitChangeIntentPattern, normalizedText, perl = TRUE)) {
    result$category = "unit_change_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$message = "Unit-change interpretation request captured for deterministic follow-up handling."
    result$unitChangeValues = unitChangeValues
    return(result)
  }

  predictionIntent = classifyPredictionQuestionIntent(normalizedText)

  if (grepl("prediction interval", normalizedText, perl = TRUE)) {
    result$category = "prediction_interval_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$predictionIntent = predictionIntent$target
    result$predictionAmbiguity = predictionIntent$ambiguity
    result$message = "Prediction-interval request captured for deterministic follow-up handling."
    return(result)
  }

  if (grepl("\\b(predict|predicted|prediction)\\b", normalizedText, perl = TRUE)) {
    result$category = "prediction_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$predictionIntent = predictionIntent$target
    result$predictionAmbiguity = predictionIntent$ambiguity
    result$message = "Prediction-style request captured for deterministic follow-up handling."
    return(result)
  }

  if (identical(predictionIntent$target, "ambiguous_personal")) {
    result$category = "prediction_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$predictionIntent = predictionIntent$target
    result$predictionAmbiguity = predictionIntent$ambiguity
    result$message = "Ambiguous personal-outcome request captured; WMFM will report both average-response and individual prediction uncertainty."
    return(result)
  }

  expectedPredictionPattern = paste(
    c(
      "\\bwhat\\b.*\\b(frequency|count|number|probability|odds|chance|value|response|mark|score|price)\\b.*\\b(expect|expected)\\b",
      "\\bwhat\\b.*\\b(expect|expected)\\b.*\\b(frequency|count|number|probability|odds|chance|value|response|mark|score|price)\\b",
      "\\bwhat\\s+is\\s+the\\s+expected\\b",
      "\\bwhat\\b.*\\bshould\\s+(i|we)\\s+expect\\b",
      "\\bshould\\s+(i|we)\\s+expect\\b",
      "\\bhow many\\b.*\\b(expect|expected)\\b",
      "\\bhow much\\b.*\\b(expect|expected)\\b",
      "\\bwhat happens\\b.*\\b(if|when)\\b",
      "\\bwhat would happen\\b.*\\b(if|when)\\b",
      "\\b(if|for|when|with|where)\\b.*\\b[[:alpha:]_][[:alnum:]_.-]*\\s+is\\s+[[:alnum:]_.-]+\\b"
    ),
    collapse = "|"
  )
  hasPredictionCondition = grepl(
    "\\b(if|for|when|with|where|at|who|weighing|scoring|scored|attending|taken)\\b",
    normalizedText,
    perl = TRUE
  ) || grepl(
    "\\b(for|when|with|where)\\b.*\\b[A-Za-z][A-Za-z0-9_.]*\\s*=",
    originalText,
    perl = TRUE
  ) || grepl(
    "\\b(if|for|when|with|where|at)\\b.*\\b[A-Za-z][A-Za-z0-9_.-]*\\s+is\\s+[A-Za-z0-9_.-]+\\b",
    normalizedText,
    perl = TRUE
  ) || grepl(
    "\\b(if|for|when|with|where|at|taken)\\b.*\\b(value|level|group|category|setting|settings|condition|conditions|case|observation|sample|site|magnitude)\\b",
    normalizedText,
    perl = TRUE
  )
  if (grepl(expectedPredictionPattern, normalizedText, perl = TRUE) && isTRUE(hasPredictionCondition)) {
    result$category = "prediction_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$message = "Expected-value prediction-style request captured for deterministic follow-up handling."
    return(result)
  }

  if (grepl("\\b(confidence interval|uncertainty|precision|how sure|how certain)\\b", normalizedText, perl = TRUE)) {
    result$category = "emphasis_uncertainty"
    result$supported = TRUE
    result$message = "Uncertainty-emphasis preference captured."
    return(result)
  }

  if (grepl("\\b(effect size|magnitude|how big|size of the effect|size of effect)\\b", normalizedText, perl = TRUE)) {
    result$category = "emphasis_effect_size"
    result$supported = TRUE
    result$message = "Effect-size emphasis preference captured."
    return(result)
  }

  if (grepl("\\b(practical|real[- ]world|in practice|practically)\\b", normalizedText, perl = TRUE)) {
    result$category = "emphasis_practical_interpretation"
    result$supported = TRUE
    result$message = "Practical-interpretation preference captured."
    return(result)
  }

  if (grepl("\\b(compare|comparison|difference between|focus on the comparison|versus|vs\\.?)\\b", normalizedText, perl = TRUE)) {
    comparisonUnitChangeValues = extractRequestedUnitChangeValues(normalizedText)
    if (length(comparisonUnitChangeValues) > 1L) {
      result$category = "unsupported_or_out_of_scope"
      result$supported = FALSE
      result$reason = "ambiguous_predictor_values"
      result$message = "Unsupported ambiguous multi-unit follow-up request; clarification required."
      result$unitChangeValues = comparisonUnitChangeValues
      return(result)
    }

    result$category = "emphasis_group_comparison"
    result$supported = TRUE
    result$message = "Group-comparison emphasis preference captured."
    return(result)
  }

  if (grepl("\\b(interaction|interact|effect modification|moderation|moderator)\\b", normalizedText, perl = TRUE)) {
    result$category = "emphasis_interaction"
    result$supported = TRUE
    result$message = "Interaction-emphasis preference captured."
    return(result)
  }

  if (grepl("\\b(beginner|novice|audience|plain english|non-technical)\\b", normalizedText, perl = TRUE)) {
    result$category = "beginner_friendly"
    result$supported = TRUE
    result$message = "Beginner-friendly preference captured."
    return(result)
  }

  if (grepl("\\b(brief|briefly|concise|shorter|summari[sz]e|keep the answer short|keep it short)\\b", normalizedText, perl = TRUE)) {
    result$category = "concise_answer"
    result$supported = TRUE
    result$message = "Concise-answer preference captured."
    return(result)
  }

  if (grepl("\\b(research question|main question|original question|focus on the question)\\b", normalizedText, perl = TRUE)) {
    result$category = "focus_research_question"
    result$supported = TRUE
    result$message = "Research-question focus preference captured."
    return(result)
  }

  result
}

#' @keywords internal
#' @noRd
#' @keywords internal
#' @noRd
extractRequestedProportionalChangeValues = function(normalizedText) {
  text = tolower(trimws(as.character(normalizedText %||% "")))
  if (!nzchar(text)) {
    return(numeric(0))
  }

  if (grepl("\\b(doubling|double|doubles|twice)\\b", text, perl = TRUE)) {
    return(100)
  }

  pattern = paste(
    c(
      "\\b(\\d+(?:\\.\\d+)?)\\s*%\\s*(?:increase|change|larger|higher|more)\\b",
      "\\b(?:increase|change)\\s+(?:by\\s+)?(\\d+(?:\\.\\d+)?)\\s*%\\b",
      "\\b(\\d+(?:\\.\\d+)?)\\s*(?:percent|percentage)\\s*(?:increase|change|larger|higher|more)\\b"
    ),
    collapse = "|"
  )
  matches = gregexpr(pattern, text, perl = TRUE)
  matchedText = regmatches(text, matches)[[1]]

  if (!length(matchedText) || identical(matchedText, "-1")) {
    return(numeric(0))
  }

  values = vapply(matchedText, function(x) {
    numericMatch = regmatches(x, gregexpr("\\d+(?:\\.\\d+)?", x, perl = TRUE))[[1]]
    if (!length(numericMatch)) {
      return(NA_real_)
    }
    as.numeric(numericMatch[[1]])
  }, numeric(1))

  values = unique(values[is.finite(values)])
  values[values > 0]
}

extractRequestedUnitChangeValues = function(normalizedText) {
  text = tolower(trimws(as.character(normalizedText %||% "")))
  if (!nzchar(text)) {
    return(numeric(0))
  }

  pattern = paste(
    c(
      "\\b(?:for\\s+)?(?:a\\s+)?(\\d+(?:\\.\\d+)?)\\s*[- ]?unit\\s+(?:increase|change)\\b",
      "\\b(?:for\\s+)?(?:a\\s+)?(\\d+(?:\\.\\d+)?)\\s*[- ]?(?:[[:alpha:]][[:alnum:]_.-]*\\s+)?(?:increase|change)\\b",
      "\\b(\\d+(?:\\.\\d+)?)\\s*[- ]?(?:unit|point)\\b",
      "\\bincrease\\s+of\\s+(\\d+(?:\\.\\d+)?)\\b",
      "\\bchange\\s+of\\s+(\\d+(?:\\.\\d+)?)\\b"
    ),
    collapse = "|"
  )
  matches = gregexpr(pattern, text, perl = TRUE)
  matchedText = regmatches(text, matches)[[1]]

  if (!length(matchedText) || identical(matchedText, "-1")) {
    return(numeric(0))
  }

  values = vapply(matchedText, function(x) {
    numericMatch = regmatches(x, gregexpr("\\d+(?:\\.\\d+)?", x, perl = TRUE))[[1]]
    if (!length(numericMatch) || identical(numericMatch, "-1")) {
      return(NA_real_)
    }
    suppressWarnings(as.numeric(numericMatch[[1]]))
  }, numeric(1))

  values = values[is.finite(values)]
  if (!length(values)) {
    return(numeric(0))
  }

  sort(unique(values))
}


#' Classify prediction intent in natural-language follow-up questions
#'
#' @param normalizedText Lower-case normalized question text.
#'
#' @return List with `target` and `ambiguity`.
#' @keywords internal
#' @noRd
classifyPredictionQuestionIntent = function(normalizedText) {
  text = as.character(normalizedText %||% "")
  hasPersonal = grepl("\\b(i|me|my|mine)\\b", text, perl = TRUE)
  hasSpecificCase = grepl("\\b(a|an|one|this)\\b.*\\b(student|person|case|observation|diamond|house|patient)\\b", text, perl = TRUE)
  hasExplicitPrediction = grepl("\\b(predict|predicted|prediction|prediction interval)\\b", text, perl = TRUE) ||
    grepl("\\bshould\\s+(i|we)\\s+expect\\b", text, perl = TRUE)
  hasAverage = grepl("\\b(on average|average|mean|expected response|expected mark|expected price|students with|people with|diamonds weighing)\\b", text, perl = TRUE)
  hasAmbiguousOutcome = grepl("\\b(will i|would i|am i likely|should i expect|do well|perform well|succeed)\\b", text, perl = TRUE)

  if (isTRUE(hasAverage) && !isTRUE(hasPersonal)) {
    return(list(target = "mean_response", ambiguity = "low"))
  }

  if (isTRUE(hasPersonal) && isTRUE(hasAmbiguousOutcome)) {
    return(list(target = "ambiguous_personal", ambiguity = "high"))
  }

  if ((isTRUE(hasPersonal) || isTRUE(hasSpecificCase)) && isTRUE(hasExplicitPrediction)) {
    return(list(target = "individual_outcome", ambiguity = "low"))
  }

  list(target = "mean_response", ambiguity = "low")
}
