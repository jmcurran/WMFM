#' Build adjustment-role metadata for fitted equation display
#'
#' Creates metadata describing which predictors are primary versus adjustment
#' variables while preserving the original fitted formula terms. This helper is
#' display-focused only and does not alter model fitting, model matrices, or
#' stored model objects.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#'
#' @return A list containing predictor grouping metadata for equation display.
#' @keywords internal
buildEquationDisplayRoleMetadata = function(model) {
  if (missing(model) || is.null(model)) {
    return(list(
      allPredictors = character(0),
      primaryPredictors = character(0),
      adjustmentPredictors = character(0),
      hasAdjustments = FALSE
    ))
  }

  modelFormula = formula(model)
  responseName = all.vars(modelFormula)[1]
  allPredictors = setdiff(all.vars(modelFormula), responseName)
  allPredictors = unique(allPredictors)

  adjustmentPredictors = attr(model, "wmfm_adjustment_variables") %||% character(0)
  adjustmentPredictors = unique(as.character(adjustmentPredictors))
  adjustmentPredictors = adjustmentPredictors[nzchar(adjustmentPredictors)]
  adjustmentPredictors = adjustmentPredictors[adjustmentPredictors %in% allPredictors]

  primaryPredictors = setdiff(allPredictors, adjustmentPredictors)

  list(
    allPredictors = allPredictors,
    primaryPredictors = primaryPredictors,
    adjustmentPredictors = adjustmentPredictors,
    hasAdjustments = length(adjustmentPredictors) > 0
  )
}

#' Build UI summary text for fitted equation role metadata
#'
#' Formats concise text describing which predictors are primary and which are
#' adjustment variables so the fitted-equation display can label term roles
#' without hiding any fitted terms.
#'
#' @param roleMetadata Output from \code{buildEquationDisplayRoleMetadata()}.
#'
#' @return Character vector of summary lines for display.
#' @keywords internal
buildEquationDisplayRoleSummary = function(roleMetadata) {
  if (is.null(roleMetadata) || !isTRUE(roleMetadata$hasAdjustments)) {
    return(character(0))
  }

  primaryText = if (length(roleMetadata$primaryPredictors) > 0) {
    paste(roleMetadata$primaryPredictors, collapse = ", ")
  } else {
    "(none selected)"
  }

  adjustmentText = paste(roleMetadata$adjustmentPredictors, collapse = ", ")

  c(
    paste0("Primary predictors: ", primaryText),
    paste0("Adjustment variables: ", adjustmentText),
    "Full fitted equation shown below includes both primary and adjustment terms."
  )
}
