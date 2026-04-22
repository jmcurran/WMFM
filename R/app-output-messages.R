#' Build user-facing progress text for fitted-model output generation
#'
#' Centralises app wording for the post-fit output pipeline so that the UI can
#' reflect deterministic equation generation without using old LLM request
#' language. Explanations may still be generated with a language model when one
#' is available.
#'
#' @param equationMethod Character string. One of `"deterministic"` or `"llm"`.
#' @param explanationAvailable Logical indicating whether a narrative explanation
#'   is available.
#' @param explanationRequested Logical indicating whether the app is attempting
#'   to generate an explanation.
#'
#' @return A named list of character strings for progress and notification text.
#' @keywords internal
buildAppOutputMessages = function(
    equationMethod = c("deterministic", "llm"),
    explanationAvailable = FALSE,
    explanationRequested = FALSE
) {

  equationMethod = match.arg(equationMethod)

  equationDetail = if (identical(equationMethod, "deterministic")) {
    "Building deterministic equations..."
  } else {
    "Requesting equations from the language model..."
  }

  equationCompleteDetail = if (identical(equationMethod, "deterministic")) {
    "Deterministic equations built."
  } else {
    "Equation response received."
  }

  explanationDetail = if (isTRUE(explanationRequested)) {
    "Generating narrative explanation..."
  } else {
    "Skipping narrative explanation..."
  }

  finishDetail = if (isTRUE(explanationRequested) && isTRUE(explanationAvailable)) {
    "Explanation received. Finishing..."
  } else if (isTRUE(explanationRequested)) {
    "Explanation unavailable. Finishing..."
  } else {
    "No explanation requested. Finishing..."
  }

  fallbackNotification = if (identical(equationMethod, "llm")) {
    paste(
      "The language model request for equations failed.",
      "WMFM switched to deterministic equations; the explanation may still be available."
    )
  } else {
    NULL
  }

  list(
    progressMessage = "Building fitted-model outputs...",
    equationDetail = equationDetail,
    equationCompleteDetail = equationCompleteDetail,
    explanationDetail = explanationDetail,
    updateDetail = "Updating app...",
    finishDetail = finishDetail,
    doneDetail = "Done.",
    fallbackNotification = fallbackNotification
  )
}
