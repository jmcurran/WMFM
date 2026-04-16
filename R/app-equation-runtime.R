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
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param chatProvider Optional chat provider object. Defaults to `NULL`.
#' @param useExplanationCache Logical. Passed through to `lmExplanation()` when
#'   chat is available. Defaults to `TRUE`.
#' @param equationMethod Character string giving the equation engine. Must be
#'   one of `"deterministic"` or `"llm"`. Defaults to `"deterministic"`.
#'
#' @return A named list with elements `equations`, `explanation`, and
#'   `equationMethodUsed`.
#' @keywords internal
buildAppModelOutputs = function(
    model,
    chatProvider = NULL,
    useExplanationCache = TRUE,
    equationMethod = c("deterministic", "llm")
) {

  equationMethod = match.arg(equationMethod)

  equations = NULL
  explanation = NULL
  equationMethodUsed = NULL
  equationFallbackUsed = FALSE

  if (identical(equationMethod, "deterministic")) {
    equations = getModelEquations(
      model = model,
      method = "deterministic"
    )
    equationMethodUsed = "deterministic"
  } else if (is.null(chatProvider)) {
    equations = getModelEquations(
      model = model,
      method = "deterministic"
    )
    equationMethodUsed = "deterministic"
  } else {
    equations = tryCatch(
      getModelEquations(
        model = model,
        method = "llm",
        chat = chatProvider
      ),
      error = function(e) {
        NULL
      }
    )

    if (is.null(equations)) {
      equations = getModelEquations(
        model = model,
        method = "deterministic"
      )
      equationMethodUsed = "deterministic"
      equationFallbackUsed = TRUE
    } else {
      equationMethodUsed = "llm"
    }
  }

  if (!is.null(chatProvider)) {
    explanation = tryCatch(
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

  list(
    equations = equations,
    explanation = explanation,
    equationMethodUsed = equationMethodUsed,
    equationFallbackUsed = equationFallbackUsed
  )
}
