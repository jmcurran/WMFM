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



#' Render adjustment-variable selection UI from explicit inputs
#'
#' Pure helper that builds the hidden compatibility checkbox input without
#' reading from reactive values directly.
#'
#' @param responseVariable Name of the current response variable.
#' @param factorVariables Character vector of variables in the factor bucket.
#' @param continuousVariables Character vector of variables in the continuous bucket.
#' @param selectedAdjustmentVariables Character vector currently selected as
#'   adjustment variables.
#'
#' @return A list with `ui`, `eligibleVariables`, and `selectedVariables`.
#' @keywords internal
#' @importFrom shiny checkboxGroupInput
renderAdjustmentVariablesUiFromInputs = function(
    responseVariable,
    factorVariables,
    continuousVariables,
    selectedAdjustmentVariables) {

  eligibleVariables = buildEligibleAdjustmentVariables(
    responseVariable = responseVariable,
    factorVariables = factorVariables,
    continuousVariables = continuousVariables
  )

  selectedVariables = sanitizeAdjustmentVariables(
    selectedVariables = selectedAdjustmentVariables,
    eligibleVariables = eligibleVariables
  )

  ui = NULL
  if (length(eligibleVariables) > 0) {
    ui = shiny::div(
      style = "display: none;",
      checkboxGroupInput(
        inputId = "adjustment_variables",
        label = "Adjust for this variable",
        choices = eligibleVariables,
        selected = selectedVariables
      )
    )
  }

  list(
    ui = ui,
    eligibleVariables = eligibleVariables,
    selectedVariables = selectedVariables
  )
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
renderAdjustmentVariablesUi = function(rv, responseVariable, factorVariables = NULL, continuousVariables = NULL, selectedAdjustmentVariables = NULL) {
  safeReactiveRead = function(expr, defaultValue = NULL) {
    tryCatch(expr, error = function(e) {
      defaultValue
    })
  }

  if (missing(factorVariables)) {
    factorVariables = safeReactiveRead(rv$bucketFactors, character(0))
  }
  if (missing(continuousVariables)) {
    continuousVariables = safeReactiveRead(rv$bucketContinuous, character(0))
  }
  if (missing(selectedAdjustmentVariables)) {
    selectedAdjustmentVariables = safeReactiveRead(rv$adjustmentVariables, character(0))
  }

  renderedUi = renderAdjustmentVariablesUiFromInputs(
    responseVariable = responseVariable,
    factorVariables = factorVariables,
    continuousVariables = continuousVariables,
    selectedAdjustmentVariables = selectedAdjustmentVariables
  )

  rv$adjustmentVariables = renderedUi$selectedVariables

  renderedUi$ui
}

#' Build a variable-card label with inline adjustment checkbox
#'
#' @param variableName Character scalar variable name to render.
#' @param selectedAdjustmentVariables Character vector of currently selected
#'   adjustment variables.
#'
#' @return A Shiny tag object suitable for bucket list labels.
#'
#' @keywords internal
renderBucketVariableLabel = function(variableName, selectedAdjustmentVariables) {
  isSelected = variableName %in% (selectedAdjustmentVariables %||% character(0))

  shiny::tags$div(
    class = "wmfm-bucket-variable",
    shiny::tags$span(variableName),
    shiny::tags$span(
      style = "float: right;",
        shiny::tags$label(
          style = "font-weight: normal; margin: 0;",
          title = paste(
            "Adjustment variables are included in the model to account for background variation,",
            "but are not treated as the primary findings."
          ),
        shiny::tags$input(
          type = "checkbox",
          class = "wmfm-adjustment-checkbox",
          `data-var` = variableName,
          checked = if (isTRUE(isSelected)) "checked" else NULL,
          `data-adjustment-active` = if (isTRUE(isSelected)) "1" else "0",
          onclick = "event.stopPropagation();",
          onchange = getAdjustmentCheckboxOnChangeScript()
        ),
        ""
      )
    )
  )
}



#' Build onchange JavaScript for inline adjustment checkboxes
#'
#' @return Character scalar JavaScript snippet.
#' @keywords internal
getAdjustmentCheckboxOnChangeScript = function() {
  paste(
    "var current = this.getAttribute('data-adjustment-active');",
    "this.setAttribute('data-adjustment-active', current === '1' ? '0' : '1');",
    "Shiny.setInputValue('adjustment_variables_inline',",
    "Array.from(document.querySelectorAll('.wmfm-adjustment-checkbox')).filter(function(el){ return el.getAttribute('data-adjustment-active') === '1'; }).map(function(el){",
    "return el.getAttribute('data-var');",
    "}), {priority: 'event'});"
  )
}


#' Build sanitized adjustment metadata for a fitted formula
#'
#' Restricts selected adjustment variables to non-empty predictors present in
#' the fitted formula so downstream prompts and summaries do not receive stale
#' adjustment state after UI invalidations or model-type switches.
#'
#' @param selectedVariables Character vector selected in the UI.
#' @param formulaPredictors Character vector of predictor names from the fitted formula.
#'
#' @return Character vector of sanitized adjustment-variable metadata.
#'
#' @keywords internal
buildAdjustmentMetadata = function(selectedVariables, formulaPredictors) {
  predictors = unique(as.character(formulaPredictors %||% character(0)))
  predictors = predictors[nzchar(predictors)]

  sanitizeAdjustmentVariables(
    selectedVariables = selectedVariables,
    eligibleVariables = predictors
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
