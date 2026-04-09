#' Format bad explanation output for user-facing return
#'
#' @param parsed A validated parsed response.
#' @param labelErrors Logical. Should metadata be returned?
#'
#' @return A character scalar, a named character vector, or a named list.
#' @keywords internal
formatBadExplanationOutput = function(parsed, labelErrors = FALSE) {

  explanationTexts = vapply(parsed, function(item) item$text, character(1))
  explanationNames = vapply(parsed, function(item) item$name, character(1))
  names(explanationTexts) = explanationNames

  if (!isTRUE(labelErrors)) {
    if (length(explanationTexts) == 1L) {
      return(unname(explanationTexts[[1]]))
    }

    return(explanationTexts)
  }

  errorTypes = lapply(parsed, function(item) item$errorTypes)
  names(errorTypes) = explanationNames

  severity = vapply(parsed, function(item) item$severity, character(1))
  names(severity) = explanationNames

  list(
    explanations = explanationTexts,
    errorTypes = errorTypes,
    severity = severity
  )
}
