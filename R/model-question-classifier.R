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

  if (grepl("prediction interval", normalizedText, perl = TRUE)) {
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

  if (grepl("\\b(beginner|novice|audience|for students|for a student|plain english|non-technical)\\b", normalizedText, perl = TRUE)) {
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
