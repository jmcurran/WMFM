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

  lines = c(
    "Adjustment-variable interpretation guidance:",
    paste0("Primary predictors: ", primaryText),
    paste0("Adjustment variables: ", adjustmentText),
    "Interpret primary predictors as the substantive findings of interest.",
    "Do not interpret adjustment-variable coefficients as substantive findings.",
    "Mention adjustment variables only in compact wording such as after adjusting for ... or holding adjustment variables constant.",
    "Do not present adjustment variables as causal mechanisms and do not infer causality from adjustment."
  )

  paste(lines, collapse = "\n")
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
      stats::model.frame(model),
      error = function(e) {
        NULL
      }
    )
  }

  predictorNames = if (!is.null(mf) && is.data.frame(mf) && ncol(mf) > 1) {
    names(mf)[-1]
  } else {
    all.vars(stats::formula(model))[-1]
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
