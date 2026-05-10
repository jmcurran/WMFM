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


#' Render adjustment-variable selection UI
#'
#' Builds the Stage B adjustment-variable checkbox input from current bucket
#' assignments and keeps stored selections synchronized to eligible choices.
#'
#' @param rv App reactive values object.
#' @param responseVariable Name of the current response variable.
#'
#' @return A Shiny tag object or `NULL` when no choices are available.
#'
#' @keywords internal
#'
#' @importFrom shiny checkboxGroupInput
renderAdjustmentVariablesUi = function(rv, responseVariable) {
  if (is.null(rv$data)) {
    return(NULL)
  }

  eligibleVariables = buildEligibleAdjustmentVariables(
    responseVariable = responseVariable,
    factorVariables = rv$bucketFactors,
    continuousVariables = rv$bucketContinuous
  )

  selectedVariables = sanitizeAdjustmentVariables(
    selectedVariables = rv$adjustmentVariables,
    eligibleVariables = eligibleVariables
  )

  rv$adjustmentVariables = selectedVariables

  if (length(eligibleVariables) == 0) {
    return(NULL)
  }

  checkboxGroupInput(
    inputId = "adjustment_variables",
    label = "Adjust for this variable",
    choices = eligibleVariables,
    selected = selectedVariables
  )
}

#' Sync selected adjustment variables from user input
#'
#' Sanitizes user checkbox selections against current eligible variables and
#' stores the synchronized values in reactive state.
#'
#' @param rv App reactive values object.
#' @param responseVariable Name of the current response variable.
#' @param selectedVariables Character vector selected by the user.
#'
#' @return No return value; updates `rv$adjustmentVariables`.
#'
#' @keywords internal
syncAdjustmentVariablesSelection = function(rv, responseVariable, selectedVariables) {
  eligibleVariables = buildEligibleAdjustmentVariables(
    responseVariable = responseVariable,
    factorVariables = rv$bucketFactors,
    continuousVariables = rv$bucketContinuous
  )

  rv$adjustmentVariables = sanitizeAdjustmentVariables(
    selectedVariables = selectedVariables,
    eligibleVariables = eligibleVariables
  )
}
