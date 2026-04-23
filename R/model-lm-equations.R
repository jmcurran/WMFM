#' Get fitted-model equations using either the deterministic engine or the language model
#'
#' Uses the legacy language-model path by default for backward compatibility.
#' Higher-level entry points such as `getModelEquations()` and `runModel()` now
#' explicitly request deterministic equations by default.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param chat Optional chat provider object as returned by
#'   \code{getChatProvider()}. Required when \code{method = "llm"}.
#' @param method Character string giving the equation engine. Must be one of
#'   \code{"llm"} or \code{"deterministic"}.
#' @param digits Integer number of decimal places for displayed coefficients in
#'   the deterministic path. Defaults to \code{2}.
#'
#' @return Either a deterministic equation table or the existing language-model
#'   equation object.
#' @keywords internal
#'
#' @importFrom stats formula model.frame
lmEquations = function(
    model,
    chat = NULL,
    method = c("llm", "deterministic"),
    digits = 2
) {

  method = match.arg(method)

  if (identical(method, "deterministic")) {
    return(buildDeterministicEquationTable(model = model, digits = digits))
  }

  warning(
    "LLM-based equation generation is deprecated; deterministic equations are now the default.",
    call. = FALSE
  )

  if (is.null(chat)) {
    stop("`chat` must be supplied when `method = \"llm\"`.", call. = FALSE)
  }

  formulaStr = paste(deparse(formula(model)), collapse = " ")
  coefStr = paste(coef(model), collapse = ";")
  mf = stats::model.frame(model)
  predictors = names(mf)[-1]
  numericAnchorInfo = buildModelNumericAnchorInfo(
    model = model,
    mf = mf,
    predictorNames = predictors
  )

  key = paste(
    "eq",
    "v2-numeric-anchor",
    formulaStr,
    coefStr,
    numericAnchorInfo$cacheKey
  )

  if (!is.null(.env_cache[[key]])) {
    return(.env_cache[[key]])
  }

  prompt = lmToPrompt(model)

  if (inherits(chat, "ProviderOpenAI")) {
    result = chat$chat_structured(prompt, type = typeLmEquations)
    output = as_tibble(result$equations)
  } else {
    output = chat$chat(prompt)
  }

  .env_cache[[key]] = output
  output
}
