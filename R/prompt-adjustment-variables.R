#' Build adjustment-variable guidance for explanation prompts
#'
#' Builds a prompt block that separates primary predictors from adjustment
#' variables using Stage 20 adjustment metadata stored on the fitted model.
#' The block instructs downstream explanation generation to interpret primary
#' predictors substantively while mentioning adjustment variables only as
#' adjusted-for controls.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A character scalar containing adjustment-variable guidance, or an
#'   empty string when no adjustment variables are present.
#' @keywords internal
#' @importFrom stats formula model.frame
buildAdjustmentVariablePromptBlock = function(model, mf = NULL) {

  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)

  if (!isTRUE(roleMetadata$hasAdjustments)) {
    return("")
  }

  primaryText = if (length(roleMetadata$primaryPredictors) > 0) {
    paste(roleMetadata$primaryPredictors, collapse = ", ")
  } else {
    "(none listed)"
  }

  adjustmentText = paste(roleMetadata$adjustmentPredictors, collapse = ", ")

  adjustmentPhrase = paste0("after adjusting for ", adjustmentText)
  omittedTerms = getAdjustmentRelatedTermsForExplanation(model = model, mf = mf)
  omittedTermsText = if (length(omittedTerms) > 0) {
    paste(omittedTerms, collapse = ", ")
  } else {
    "(none)"
  }

  hasAdjustmentInteractions = any(grepl(":", omittedTerms, fixed = TRUE))

  lines = c(
    "Adjustment-variable interpretation policy (version: stage20.13-v2):",
    paste0("Response variable: ", names(mf %||% stats::model.frame(model))[[1]]),
    paste0("Primary predictors: ", primaryText),
    paste0("Adjustment variables: ", adjustmentText),
    paste0("Omitted adjustment-related terms in explanation payload: ", omittedTermsText),
    paste0("Frame the main answer around primary predictors ", adjustmentPhrase, "."),
    "Answer the research question using the variables of scientific interest (primary predictors).",
    "Interpret primary predictors as the substantive findings of interest.",
    "Mention adjustment variables only in compact wording such as after adjusting for ... or after accounting for ....",
    "Do not interpret adjustment-variable coefficients as substantive findings.",
    "Do not interpret interaction terms that include any adjustment variable as main findings.",
    "Do not discuss results at individual levels of adjustment variables.",
    "Do not report picture-specific means, contrasts, confidence intervals, or coefficient estimates.",
    "Do not use adjustment-variable levels as narrative examples.",
    "Do not provide adjustment-level-specific or picture-specific effect estimates or subgroup narration.",
    "For interactions involving adjustment variables, use only a high-level model-structure note if needed; do not narrate individual levels or coefficients.",
    "Do not present adjustment variables as causal mechanisms and do not infer causality from adjustment."
  )

  if (isTRUE(hasAdjustmentInteractions)) {
    lines = c(
      lines,
      "Model-structure note: The fitted model includes interaction terms involving adjustment variables, so adjusted comparisons may vary across adjustment levels, but those terms are not interpreted as the main findings."
    )
  }

  paste(lines, collapse = "\n")
}

#' Get adjustment-related terms for explanation-policy omission metadata
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return Character vector of model terms involving adjustment variables.
#' @keywords internal
getAdjustmentRelatedTermsForExplanation = function(model, mf = NULL) {
  roleMetadata = getAdjustmentRoleMetadataForExplanation(model = model, mf = mf)
  if (!isTRUE(roleMetadata$hasAdjustments)) {
    return(character(0))
  }

  termLabels = attr(stats::terms(model), "term.labels") %||% character(0)
  termLabels = unique(as.character(termLabels))
  termLabels[nzchar(termLabels) & vapply(
    termLabels,
    termInvolvesAdjustmentVariable,
    logical(1),
    adjustmentVariables = roleMetadata$adjustmentPredictors
  )]
}

#' Get explanation-role metadata for primary and adjustment predictors
#'
#' Uses fitted-model predictor names plus Stage 20 adjustment metadata to build
#' a consistent role split for explanation and prompt helpers.
#'
#' @param model A fitted model object.
#' @param mf Optional model frame.
#'
#' @return A list with `primaryPredictors`, `adjustmentPredictors`, and
#'   `hasAdjustments` fields.
#' @keywords internal
getAdjustmentRoleMetadataForExplanation = function(model, mf = NULL) {

  if (is.null(mf)) {
    mf = tryCatch(
      model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  predictorNames = if (!is.null(mf) && is.data.frame(mf) && ncol(mf) > 1) {
    names(mf)[-1]
  } else {
    all.vars(formula(model))[-1]
  }

  predictorNames = unique(as.character(predictorNames))
  predictorNames = predictorNames[nzchar(predictorNames)]

  adjustmentPredictors = attr(model, "wmfm_adjustment_variables", exact = TRUE) %||% character(0)
  adjustmentPredictors = unique(as.character(adjustmentPredictors))
  adjustmentPredictors = adjustmentPredictors[nzchar(adjustmentPredictors)]
  adjustmentPredictors = adjustmentPredictors[adjustmentPredictors %in% predictorNames]

  primaryPredictors = setdiff(predictorNames, adjustmentPredictors)

  list(
    primaryPredictors = primaryPredictors,
    adjustmentPredictors = adjustmentPredictors,
    hasAdjustments = length(adjustmentPredictors) > 0
  )
}
