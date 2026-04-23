buildAppEquations = function(
    model,
    chatProvider = NULL,
    equationMethod = c("deterministic", "llm")
) {

  equationMethod = match.arg(equationMethod)
  equationFallbackUsed = FALSE

  if (identical(equationMethod, "deterministic")) {
    return(list(
      equations = getModelEquations(
        model = model,
        method = "deterministic"
      ),
      equationMethodUsed = "deterministic",
      equationFallbackUsed = FALSE
    ))
  }

  if (is.null(chatProvider)) {
    return(list(
      equations = getModelEquations(
        model = model,
        method = "deterministic"
      ),
      equationMethodUsed = "deterministic",
      equationFallbackUsed = TRUE
    ))
  }

  llmEquations = tryCatch(
    getModelEquations(
      model = model,
      method = "llm",
      chat = chatProvider
    ),
    error = function(e) {
      NULL
    }
  )

  if (!is.null(llmEquations)) {
    return(list(
      equations = llmEquations,
      equationMethodUsed = "llm",
      equationFallbackUsed = FALSE
    ))
  }

  list(
    equations = getModelEquations(
      model = model,
      method = "deterministic"
    ),
    equationMethodUsed = "deterministic",
    equationFallbackUsed = TRUE
  )
}

#' Build app-side explanation text for a fitted model
#'
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param chatProvider Optional chat provider object. Defaults to `NULL`.
#' @param useExplanationCache Logical. Passed through to `lmExplanation()` when
#'   chat is available. Defaults to `TRUE`.
#'
#' @return A character string containing the generated explanation, or `NULL`
#'   if no explanation could be produced.
#' @keywords internal
buildAppExplanation = function(model, chatProvider = NULL, useExplanationCache = TRUE) {

  if (is.null(chatProvider)) {
    return(NULL)
  }

  tryCatch(
    lmExplanation(
      model = model,
      chat = chatProvider,
      useCache = useExplanationCache
    ),
    error = function(e) {
      NULL
    }
  )
}

#' Build app-side explanation audit output for a fitted model
#'
#' Creates the deterministic audit trail used by the transparency panel in the
#' app. This does not depend on an active chat provider.
#'
#' The returned object is expected to match the same contract used by
#' `buildModelExplanationAudit()` so that later UI and teaching-summary helpers
#' can rely on one stable audit structure.
#'
#' @param model A fitted model object, typically of class `lm` or `glm`.
#'
#' @return A `wmfmExplanationAudit` object or `NULL` if audit construction
#'   fails.
#' @keywords internal
buildAppExplanationAudit = function(model) {

  tryCatch(
    {
      audit = buildModelExplanationAudit(model = model)
      validateWmfmExplanationAudit(x = audit)
      audit
    },
    error = function(e) {
      NULL
    }
  )
}

#' Build app-side equation and explanation outputs for a fitted model
#'
#' Centralises the logic used by the app after a model has been fitted.
#' Deterministic equations are now the default app output. When explicitly
#' requested, the older LLM equation path can still be used while keeping a
#' deterministic fallback if the LLM equation request fails.
#'
#' Explanations remain on the LLM path. If no chat provider is available,
#' deterministic equations are still returned and the explanation remains empty.
#'
#' The explanation audit is always built from the deterministic model-side audit
#' helper rather than from separate app-specific audit logic.
#'
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param chatProvider Optional chat provider object. Defaults to `NULL`.
#' @param useExplanationCache Logical. Passed through to `lmExplanation()` when
#'   chat is available. Defaults to `TRUE`.
#' @param equationMethod Character string giving the equation engine. Must be
#'   one of `"deterministic"` or `"llm"`. Defaults to `"deterministic"`.
#'
#' @return A named list with elements `equations`, `explanation`,
#'   `explanationAudit`, and `equationMethodUsed`.
#' @keywords internal
buildAppModelOutputs = function(
    model,
    chatProvider = NULL,
    useExplanationCache = TRUE,
    equationMethod = c("deterministic", "llm")
) {

  equationResults = buildAppEquations(
    model = model,
    chatProvider = chatProvider,
    equationMethod = equationMethod
  )

  explanation = buildAppExplanation(
    model = model,
    chatProvider = chatProvider,
    useExplanationCache = useExplanationCache
  )

  explanationAudit = buildAppExplanationAudit(model = model)

  list(
    equations = equationResults$equations,
    explanation = explanation,
    explanationAudit = explanationAudit,
    equationMethodUsed = equationResults$equationMethodUsed,
    equationFallbackUsed = equationResults$equationFallbackUsed
  )
}
