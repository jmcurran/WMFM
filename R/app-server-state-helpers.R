#' Create app-server state helper closures
#'
#' Bundles small server-local state helper closures so the top-level app server
#' can stay focused on wiring feature sections together.
#'
#' @param input Shiny input object.
#' @param session Shiny session object.
#' @param rv Server reactive values object.
#' @param modelFit Reactive value storing the fitted model.
#'
#' @return A named list of helper functions.
#'
#' @importFrom shiny updateCheckboxGroupInput
#'
#' @keywords internal
#' @noRd
createAppServerStateHelpers = function(input, session, rv, modelFit) {
  setBucketState = function(factors = NULL, continuous = NULL) {
    vars = rv$allVars %||% character(0)

    nextFactors = intersect(factors %||% character(0), vars)
    nextContinuous = intersect(continuous %||% character(0), vars)

    currentFactors = rv$bucketFactors %||% character(0)
    currentContinuous = rv$bucketContinuous %||% character(0)

    changed = FALSE

    if (!identical(currentFactors, nextFactors)) {
      rv$bucketFactors = nextFactors
      changed = TRUE
    }

    if (!identical(currentContinuous, nextContinuous)) {
      rv$bucketContinuous = nextContinuous
      changed = TRUE
    }

    invisible(changed)
  }

  resetModelPage = function(resetResponse = TRUE) {
    rv$isResetting = TRUE
    on.exit({
      rv$isResetting = FALSE
    }, add = TRUE)

    modelFit(NULL)
    rv$modelEquations = NULL
    rv$modelExplanation = NULL
    rv$modelExplanationAudit = NULL
    rv$modelExplanationTutor = NULL
    rv$modelExplanationMessage = NULL
    rv$modelContext = NULL
    rv$analysisRecipe = NULL

    rv$autoFormula = ""
    rv$lastResponse = NULL
    rv$lastFactors = character(0)
    rv$pendingFactorVar = NULL
    rv$pendingExampleInteractions = character(0)
    rv$adjustmentVariables = character(0)

    setBucketState(
      factors = character(0),
      continuous = character(0)
    )

    rv$bucketGroupId = rv$bucketGroupId + 1L

    updateSelectInput(session, "model_type", selected = "lm")

    freezeReactiveValue(input, "factors")
    freezeReactiveValue(input, "continuous")
    freezeReactiveValue(input, "interactions")
    freezeReactiveValue(input, "adjustment_variables")
    freezeReactiveValue(input, "formula_text")
    freezeReactiveValue(input, "response_var")

    updateTextInput(session, "formula_text", value = "")
    updateSelectInput(session, "interactions", selected = character(0))
    updateCheckboxGroupInput(session, "adjustment_variables", selected = character(0))
    updateTextInput(session, "researchQuestion", value = rv$researchQuestion %||% "")

    if (resetResponse && !is.null(rv$data) && length(rv$allVars) > 0) {
      updateSelectInput(
        session,
        "response_var",
        choices = rv$allVars,
        selected = rv$allVars[1]
      )
    }
  }

  applyLoadedExampleToInputs = function(exampleInfo) {
    spec = exampleInfo$spec %||% list()
    exampleFormula = stats::as.formula(spec$formula)
    responseVar = all.vars(exampleFormula[[2]])[1] %||% rv$allVars[1]
    termLabels = attr(stats::terms(exampleFormula), "term.labels") %||% character(0)
    interactionTerms = termLabels[grepl(":", termLabels, fixed = TRUE)]
    rhsVars = all.vars(exampleFormula[[3]])
    mainEffectTerms = unique(setdiff(rhsVars, responseVar))

    factorVars = mainEffectTerms[vapply(mainEffectTerms, function(varName) {
      column = rv$data[[varName]]
      is.factor(column) || is.character(column)
    }, logical(1))]

    continuousVars = setdiff(mainEffectTerms, factorVars)

    setBucketState(
      factors = intersect(factorVars, rv$allVars),
      continuous = intersect(continuousVars, rv$allVars)
    )
    rv$bucketGroupId = rv$bucketGroupId + 1L
    rv$lastFactors = rv$bucketFactors
    rv$lastResponse = responseVar
    rv$pendingExampleInteractions = interactionTerms

    selectedAdjustmentVariables = spec$adjustmentVariables %||% character(0)
    selectedAdjustmentVariables = intersect(selectedAdjustmentVariables, mainEffectTerms)
    rv$adjustmentVariables = intersect(selectedAdjustmentVariables, rv$allVars)

    updateSelectInput(
      session,
      "response_var",
      choices = rv$allVars,
      selected = responseVar
    )

    updateSelectInput(
      session,
      "model_type",
      selected = spec$modelType %||% "lm"
    )

    updateSelectInput(
      session,
      "interactions",
      selected = interactionTerms
    )

    updateCheckboxGroupInput(
      session,
      "adjustment_variables",
      selected = rv$adjustmentVariables
    )

    exampleFormulaText = spec$formula %||% ""
    formulaHasTransformCall = grepl("(", exampleFormulaText, fixed = TRUE)

    if (isTRUE(formulaHasTransformCall)) {
      rv$autoFormula = ""
    } else {
      rv$autoFormula = exampleFormulaText
    }

    updateTextInput(session, "formula_text", value = exampleFormulaText)
    session$onFlushed(function() {
      if (isTRUE(formulaHasTransformCall)) {
        rv$autoFormula = ""
      } else {
        rv$autoFormula = exampleFormulaText
      }
      updateTextInput(session, "formula_text", value = exampleFormulaText)
    }, once = TRUE)
    exampleResearchQuestion = exampleInfo$researchQuestion %||% ""
    exampleFollowupQuestion = exampleInfo$followupQuestion %||% ""

    rv$researchQuestion = trimws(exampleResearchQuestion)
    rv$modelFollowupQuestion = trimws(exampleFollowupQuestion)

    updateTextInput(session, "researchQuestion", value = exampleResearchQuestion)
    shiny::updateTextAreaInput(session, "modelFollowupQuestion", value = exampleFollowupQuestion)
    session$onFlushed(function() {
      updateTextInput(session, "researchQuestion", value = exampleResearchQuestion)
      shiny::updateTextAreaInput(session, "modelFollowupQuestion", value = exampleFollowupQuestion)
    }, once = TRUE)
  }

  list(
    setBucketState = setBucketState,
    resetModelPage = resetModelPage,
    applyLoadedExampleToInputs = applyLoadedExampleToInputs
  )
}
