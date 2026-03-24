#' Get a plain-language explanation via the language model
#'
#' Calls the chat provider with a prompt constructed from a fitted model and
#' returns a narrative explanation of the model.
#'
#' By default, results are cached based on the model formula and coefficients
#' so that repeated requests for the same fitted model do not repeatedly query
#' the language model. This behaviour can be disabled by setting
#' `useCache = FALSE`, which is useful when deliberately testing variation
#' across repeated language model calls.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param chat A chat provider object as returned by \code{getChatProvider()}.
#' @param useCache Logical. Should explanation results be cached and reused for
#'   identical fitted models? Defaults to `TRUE`.
#'
#' @return A character scalar containing the explanation text returned by the
#'   language model.
#' @keywords internal
lmExplanation = function(model, chat, useCache = TRUE) {

  if (!is.logical(useCache) || length(useCache) != 1 || is.na(useCache)) {
    stop("`useCache` must be TRUE or FALSE.", call. = FALSE)
  }

  formulaStr = paste(deparse(formula(model)), collapse = " ")
  coefStr = paste(coef(model), collapse = ";")
  key = paste("expl", formulaStr, coefStr)

  if (isTRUE(useCache) && !is.null(.env_cache[[key]])) {
    return(.env_cache[[key]])
  }

  prompt = lmToExplanationPrompt(model)
  output = chat$chat(prompt)

  if (isTRUE(useCache)) {
    .env_cache[[key]] = output
  }

  output
}
