#' Generate bad explanations with an LLM
#'
#' @param x A `wmfmModel` object.
#' @param baseExplanation Character scalar giving the good explanation.
#' @param plan A plan object produced by `buildBadExplanationPlan()`.
#' @param chat A chat provider object with a callable `$chat()` method.
#' @param showProgress Logical. Should a command-line status message be shown
#'   before awaiting the LLM response?
#'
#' @return Raw character response from the LLM.
#' @keywords internal
generateBadExplanationWithLlm = function(
    x,
    baseExplanation,
    plan,
    chat,
    showProgress = FALSE
) {

  if (!is.logical(showProgress) || length(showProgress) != 1 || is.na(showProgress)) {
    stop("`showProgress` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.null(chat)) {
    stop("`chat` must not be NULL.", call. = FALSE)
  }

  chatMethod = tryCatch(
    chat$chat,
    error = function(e) {
      NULL
    }
  )

  if (is.null(chatMethod) || !is.function(chatMethod)) {
    stop(
      "`chat` must provide a callable `$chat()` method.",
      call. = FALSE
    )
  }

  prompt = buildBadExplanationPrompt(
    x = x,
    baseExplanation = baseExplanation,
    plan = plan
  )

  if (isTRUE(showProgress)) {
    cat("  Awaiting LLM response...\n")
  }

  rawResponse = chatMethod(prompt)
  safeWmfmScalar(rawResponse, naString = "")
}
