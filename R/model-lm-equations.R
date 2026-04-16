#' Get fitted-model equations using either the language model or the deterministic engine
#'
#' Uses the existing language-model equation path by default, with caching based
#' on the fitted model. A deterministic rendering path can also be selected for
#' development and transition work.
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
#' @return Either the existing language-model equation object or a deterministic
#'   equation table.
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
