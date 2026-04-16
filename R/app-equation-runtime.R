#' Build app-side equation and explanation outputs for a fitted model
#'
#' Centralises the logic used by the app after a model has been fitted. When a
#' chat provider is available, the app keeps using the existing language-model
#' path for equations and explanations. When chat is unavailable, the app falls
#' back to deterministic equation rendering while leaving the explanation empty.
#'
#' If the language-model equation request fails even though chat is available,
#' this helper also falls back to deterministic equations so the user still sees
#' fitted equations in the app.
#'
#' @param model A fitted model object, typically of class `lm` or `glm`.
#' @param chatProvider Optional chat provider object. Defaults to `NULL`.
#' @param useExplanationCache Logical. Passed through to `lmExplanation()` when
#'   chat is available. Defaults to `TRUE`.
#'
#' @return A named list with elements `equations`, `explanation`, and
#'   `equationMethodUsed`.
#' @keywords internal
buildAppModelOutputs = function(
    model,
    chatProvider = NULL,
    useExplanationCache = TRUE
) {

  equations = NULL
  explanation = NULL
  equationMethodUsed = NULL

  if (is.null(chatProvider)) {
    equations = getModelEquations(
      model = model,
      method = "deterministic"
    )
    equationMethodUsed = "deterministic"

    return(list(
      equations = equations,
      explanation = explanation,
      equationMethodUsed = equationMethodUsed
    ))
  }

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
  } else {
    equationMethodUsed = "llm"
  }

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

  list(
    equations = equations,
    explanation = explanation,
    equationMethodUsed = equationMethodUsed
  )
}
