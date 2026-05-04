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
    rv$modelContext = NULL

    rv$autoFormula = ""
    rv$lastResponse = NULL
    rv$lastFactors = character(0)
    rv$pendingFactorVar = NULL
    rv$pendingExampleInteractions = character(0)

    setBucketState(
      factors = character(0),
      continuous = character(0)
    )

    rv$bucketGroupId = rv$bucketGroupId + 1L

    updateSelectInput(session, "model_type", selected = "lm")

    freezeReactiveValue(input, "factors")
    freezeReactiveValue(input, "continuous")
    freezeReactiveValue(input, "interactions")
    freezeReactiveValue(input, "formula_text")
    freezeReactiveValue(input, "response_var")

    updateTextInput(session, "formula_text", value = "")
    updateSelectInput(session, "interactions", selected = character(0))
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
    mainEffectTerms = termLabels[!grepl(":", termLabels, fixed = TRUE)]

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

    rv$autoFormula = spec$formula %||% ""
    updateTextInput(session, "formula_text", value = spec$formula %||% "")
    updateTextInput(session, "researchQuestion", value = exampleInfo$researchQuestion %||% "")
  }

  list(
    setBucketState = setBucketState,
    resetModelPage = resetModelPage,
    applyLoadedExampleToInputs = applyLoadedExampleToInputs
  )
}
