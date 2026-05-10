#' Build eligible adjustment variables from predictor buckets
#'
#' Returns factor and numeric predictors that are currently eligible to be
#' marked as adjustment variables. The response variable is always excluded.
#'
#' @param responseVariable Name of the current response variable.
#' @param factorVariables Character vector of variables in the factor bucket.
#' @param continuousVariables Character vector of variables in the continuous bucket.
#'
#' @return Character vector of eligible adjustment variables.
#'
#' @keywords internal
buildEligibleAdjustmentVariables = function(responseVariable, factorVariables, continuousVariables) {
  predictorVariables = unique(c(
    factorVariables %||% character(0),
    continuousVariables %||% character(0)
  ))

  setdiff(predictorVariables, responseVariable %||% "")
}

#' Sanitize user-selected adjustment variables
#'
#' Restricts user-selected adjustment variables to currently eligible variables,
#' removes duplicates, and drops empty values.
#'
#' @param selectedVariables Character vector selected in the UI.
#' @param eligibleVariables Character vector of eligible adjustment variables.
#'
#' @return Character vector of sanitized adjustment variables.
#'
#' @keywords internal
sanitizeAdjustmentVariables = function(selectedVariables, eligibleVariables) {
  selected = unique(as.character(selectedVariables %||% character(0)))
  selected = selected[nzchar(selected)]

  intersect(selected, eligibleVariables %||% character(0))
}
