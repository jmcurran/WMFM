#' Normalize scoring role variables
#'
#' @param x Character vector, list, or scalar of role-variable names.
#'
#' @return Character vector of unique non-empty role-variable names.
#' @keywords internal
normaliseWmfmScoringRoleVariables = function(x) {
  if (is.null(x)) {
    return(character(0))
  }

  x = unlist(x, use.names = FALSE)
  x = trimws(as.character(x))
  x = x[!is.na(x) & nzchar(x)]
  unique(x)
}

#' Build adjustment-aware scoring policy metadata
#'
#' @param runRecord Named run-record list.
#'
#' @return A list containing normalized role metadata for scoring prompts.
#' @keywords internal
buildAdjustmentScoringPolicy = function(runRecord) {
  adjustmentVariables = normaliseWmfmScoringRoleVariables(runRecord$adjustmentVariables %||% character(0))
  primaryVariables = normaliseWmfmScoringRoleVariables(runRecord$primaryVariables %||% character(0))

  list(
    adjustmentVariables = adjustmentVariables,
    primaryVariables = primaryVariables,
    hasAdjustmentVariables = length(adjustmentVariables) > 0,
    hasPrimaryVariables = length(primaryVariables) > 0
  )
}

#' Build adjustment-aware scoring context text
#'
#' @param runRecord Named run-record list.
#'
#' @param policy Optional output from `buildAdjustmentScoringPolicy()`.
#'
#' @return Character scalar containing adjustment scoring context.
#' @keywords internal
buildAdjustmentScoringContextBlock = function(runRecord, policy = NULL) {
  if (is.null(policy)) {
    policy = buildAdjustmentScoringPolicy(runRecord = runRecord)
  }

  if (!isTRUE(policy$hasAdjustmentVariables)) {
    return("No adjustment-variable context was supplied. Use the standard completeness and interaction expectations.")
  }

  primaryText = if (isTRUE(policy$hasPrimaryVariables)) {
    paste0("Variables of scientific interest: ", paste(policy$primaryVariables, collapse = ", "))
  } else {
    "Variables of scientific interest: (not supplied)"
  }

  paste(
    "Adjustment-variable context is present.",
    paste0("Adjustment variables: ", paste(policy$adjustmentVariables, collapse = ", ")),
    primaryText,
    "Scoring policy for adjustment-aware explanations:",
    "- Reward correct adjusted-for framing when adjustment variables are mentioned as controls or background variables.",
    "- Reward restrained explanations that answer the research question using the variables of scientific interest.",
    "- Do not penalise explanations for omitting adjustment-level details such as level-specific fitted means, contrasts, confidence intervals, coefficients, or predicted values.",
    "- Do not require picture-specific or other adjustment-level narratives.",
    "- Do not require coefficient-by-coefficient narration of adjustment variables.",
    "- If an interaction involves an adjustment variable, do not penalise the absence of interaction cell-by-cell descriptions.",
    "- Adjustment variables may be acknowledged in high-level interaction summaries, but they should not become narrative conditioning axes with level-specific estimates unless the prompt explicitly asks for that interpretation.",
    "- Penalise explanations that turn adjustment variables into the main scientific story, narrate adjustment-level cells as if they were primary findings, or recite the regression table instead of answering the research question.",
    "- Reward avoiding forbidden adjustment-level narratives while still answering the research question.",
    sep = "\n"
  )
}

#' Build adjustment-aware rubric guidance text
#'
#' @return Character vector of field-specific adjustment scoring guidance lines.
#' @keywords internal
buildAdjustmentScoringRubricGuidance = function() {
  c(
    "  When adjustment variables are supplied, do not mark this field down merely because the explanation avoids adjustment-level interaction cell narration.",
    "  When an interaction involves an adjustment variable, prefer high-level adjusted-for interpretation over level-specific adjustment-cell storytelling unless the context asks for those cells.",
    "  When adjustment variables are supplied, do not require separate substantive narration of adjustment-variable main effects.",
    "  When adjustment variables are supplied, do not require adjustment-level reference-group narration unless it is needed to answer the research question.",
    "  When adjustment variables are supplied, do not require adjustment-level comparison structures or conditioning axes unless they are needed to answer the research question."
  )
}
