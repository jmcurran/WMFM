#' Get a plain-language explanation via the language model (with caching)
#'
#' Calls the chat provider with a prompt constructed from a fitted model and
#' returns a narrative explanation of the model. Results are cached based on
#' the model formula and coefficients to avoid repeated calls for the same
#' model.
#'
#' @param model A fitted model object, typically of class \code{"lm"} or
#'   \code{"glm"}.
#' @param chat A chat provider object as returned by \code{getChatProvider()}.
#'
#' @return A character scalar containing the explanation text returned by
#'   the language model.
#' @keywords internal
lmExplanation = function(model, chat) {
  key = paste(
    "exp",
    deparse(formula(model)),
    paste(coef(model), collapse = ";")
  )

  if (!is.null(.env_cache[[key]])) {
    return(.env_cache[[key]])
  }

  prompt = lmToExplanationPrompt(model)
  output = chat$chat(prompt)
  .env_cache[[key]] = output
  output
}
