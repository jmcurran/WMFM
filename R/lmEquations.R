#' Get fitted-model equations via the language model (with caching)
#'
#' Calls the chat provider with a prompt constructed from a fitted model and
#' returns the fitted-model equations. Results are cached based on the model
#' formula and coefficients to avoid repeated calls for the same model.
#'
#' If the provider supports structured output (e.g. OpenAI via ellmer),
#' the result is converted to a tibble with \code{condition} and
#' \code{equation} columns. Otherwise, a character vector is returned.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param chat A chat provider object as returned by \code{getChatProvider()}.
#'
#' @return Either a tibble with columns \code{condition} and \code{equation},
#'   or a character vector with raw text from the language model.
#' @keywords internal
lmEquations = function(model, chat) {
  key = paste(
    "eq",
    deparse(formula(model)),
    paste(coef(model), collapse = ";")
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
