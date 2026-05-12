#' Return adjustment variables stored on a fitted model
#'
#' @param model A fitted model object.
#'
#' @return Character vector of adjustment-variable names.
#'
#' @keywords internal
getModelAdjustmentVariables = function(model) {
  adjustmentVariables = attr(model, "wmfm_adjustment_variables", exact = TRUE) %||% character(0)
  adjustmentVariables = unique(as.character(adjustmentVariables))
  adjustmentVariables[nzchar(adjustmentVariables)]
}

#' Determine whether a model-output row is adjustment-related
#'
#' A row is adjustment-related when its term label includes at least one
#' selected adjustment variable.
#'
#' @param rowLabel Character scalar row label from summary/anova output.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#'
#' @return Logical scalar.
#'
#' @keywords internal
isAdjustmentRelatedOutputRow = function(rowLabel, adjustmentVariables) {
  termInvolvesAdjustmentVariable(
    termLabel = rowLabel,
    adjustmentVariables = adjustmentVariables
  )
}

#' Filter coefficient matrix rows for summary display
#'
#' @param coefficientsMatrix Matrix from `summary(model)$coefficients`.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#' @param showAdjustmentCoefficients Logical; when `TRUE` no rows are filtered.
#'
#' @return Coefficient matrix filtered for display.
#'
#' @keywords internal
filterSummaryCoefficientRows = function(coefficientsMatrix, adjustmentVariables, showAdjustmentCoefficients = FALSE) {
  if (isTRUE(showAdjustmentCoefficients) || is.null(coefficientsMatrix) || nrow(coefficientsMatrix) == 0) {
    return(coefficientsMatrix)
  }

  rowLabels = rownames(coefficientsMatrix) %||% character(0)
  if (length(rowLabels) == 0) {
    return(coefficientsMatrix)
  }

  keepRows = !vapply(
    rowLabels,
    isAdjustmentRelatedOutputRow,
    logical(1),
    adjustmentVariables = adjustmentVariables
  )

  coefficientsMatrix[keepRows, , drop = FALSE]
}

#' Filter ANOVA/deviance rows for display
#'
#' @param anovaTable ANOVA table object.
#' @param adjustmentVariables Character vector of adjustment-variable names.
#' @param showAdjustmentTerms Logical; when `TRUE` no rows are filtered.
#'
#' @return Filtered ANOVA table.
#'
#' @keywords internal
filterAnovaTermRows = function(anovaTable, adjustmentVariables, showAdjustmentTerms = FALSE) {
  if (isTRUE(showAdjustmentTerms) || is.null(anovaTable) || nrow(anovaTable) == 0) {
    return(anovaTable)
  }

  rowLabels = rownames(anovaTable) %||% character(0)
  if (length(rowLabels) == 0) {
    return(anovaTable)
  }

  keepRows = !vapply(
    rowLabels,
    isAdjustmentRelatedOutputRow,
    logical(1),
    adjustmentVariables = adjustmentVariables
  )

  anovaTable[keepRows, , drop = FALSE]
}
