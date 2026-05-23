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
    message = "Request not supported in this stage."
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
    result$message = "Unsupported or out-of-scope follow-up request."
    return(result)
  }

  if (grepl("prediction interval|confidence interval", normalizedText, perl = TRUE)) {
    result$category = "prediction_interval_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$message = "Prediction-interval style request captured for a later stage."
    return(result)
  }

  if (grepl("\\bpredict|predicted|prediction\\b", normalizedText, perl = TRUE)) {
    result$category = "prediction_request"
    result$supported = TRUE
    result$requiresDeterministicComputation = TRUE
    result$message = "Prediction-style request captured for a later stage."
    return(result)
  }

  if (grepl("\\b(\\d+\\s*[- ]?unit|unit increase|per unit|increase of)\\b", normalizedText, perl = TRUE)) {
    result$category = "alternative_unit_change"
    result$supported = TRUE
    result$message = "Alternative unit-change framing request captured."
    return(result)
  }

  if (grepl("\\b(compare|comparison|difference between|focus on the comparison|versus|vs\\.?)\\b", normalizedText, perl = TRUE)) {
    result$category = "subgroup_or_factor_comparison"
    result$supported = TRUE
    result$message = "Subgroup or factor-comparison request captured."
    return(result)
  }

  if (grepl("\\b(beginner|novice|audience|for students|for a student|plain english|non-technical)\\b", normalizedText, perl = TRUE)) {
    result$category = "style_or_audience"
    result$supported = TRUE
    result$message = "Style or audience preference captured."
    return(result)
  }

  if (grepl("\\b(brief|briefly|concise|shorter|summari[sz]e)\\b", normalizedText, perl = TRUE)) {
    result$category = "concise_summary"
    result$supported = TRUE
    result$message = "Concise-summary preference captured."
    return(result)
  }

  result
}
