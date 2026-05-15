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
    "Interpretation policy:",
    paste0("Response variable: ", names(mf %||% stats::model.frame(model))[[1]]),
    paste0("Primary predictors: ", primaryText),
    paste0("The following variables are adjustment variables: ", adjustmentText),
    paste0("Omitted adjustment-related terms in explanation payload: ", omittedTermsText),
    paste0("Frame the main answer around primary predictors ", adjustmentPhrase, "."),
    "The research question is about the non-adjustment variables of interest.",
    "Interpret primary predictors as the substantive findings of interest.",
    "Mention adjustment variables only in adjusted-for language such as after adjusting for ... or after accounting for ....",
    "Do not interpret adjustment-variable coefficients, contrasts, confidence intervals, fitted means, predicted values, or model terms as findings.",
    "Do not discuss results separately by levels or values of adjustment variables.",
    "Do not use adjustment variables as narrative axes.",
    "Do not interpret interactions involving adjustment variables level by level.",
    "Do not present adjustment variables as causal mechanisms and do not infer causality from adjustment."
  )

  if (isTRUE(hasAdjustmentInteractions)) {
    lines = c(
      lines,
      "Model-structure note: The fitted model includes terms involving adjustment variables, so the adjusted comparison is based on that model structure."
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
