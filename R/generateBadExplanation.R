#' Generate one or more deliberately bad model explanations
#'
#' Generates plausible but intentionally flawed explanations starting from a
#' good explanation, either supplied directly or stored in a `wmfmModel`
#' object. The generated explanations are returned in a form that can be passed
#' directly to `grade()`.
#'
#' @param x A `wmfmModel` object.
#' @param explanation Optional character scalar giving the base good
#'   explanation. If `NULL`, `x$explanation` is used.
#' @param type Character vector of bad explanation types, or `"auto"`.
#' @param severity Character scalar. One of `"subtle"`, `"moderate"`, or
#'   `"severe"`.
#' @param n Integer. Number of bad explanations to generate.
#' @param mixTypes Logical. Should individual explanations combine multiple bad
#'   types?
#' @param labelErrors Logical. Should the return value include error labels and
#'   severity metadata?
#' @param provider Optional chat provider. If `NULL`, uses `getChatProvider()`.
#' @param ... Additional arguments reserved for future use.
#'
#' @return If `n = 1` and `labelErrors = FALSE`, a character scalar.
#'
#'   If `n > 1` and `labelErrors = FALSE`, a named character vector.
#'
#'   If `labelErrors = TRUE`, a named list containing explanations, error type
#'   labels, and severity labels.
#' @export
generateBadExplanation = function(x, ...) {
  UseMethod("generateBadExplanation")
}

#' @export
generateBadExplanation.wmfmModel = function(
    x,
    explanation = NULL,
    type = "auto",
    severity = c("subtle", "moderate", "severe"),
    n = 1,
    mixTypes = FALSE,
    labelErrors = FALSE,
    provider = NULL,
    ...
) {

  validateBadExplanationRequest(
    x = x,
    explanation = explanation,
    type = type,
    severity = severity[1],
    n = n,
    mixTypes = mixTypes,
    labelErrors = labelErrors
  )

  severity = match.arg(severity)

  baseExplanation = resolveBadExplanationSource(
    x = x,
    explanation = explanation
  )

  plan = buildBadExplanationPlan(
    x = x,
    type = type,
    severity = severity,
    n = n,
    mixTypes = mixTypes
  )

  chat = provider %||% getChatProvider()

  rawResponse = generateBadExplanationWithLlm(
    x = x,
    baseExplanation = baseExplanation,
    plan = plan,
    chat = chat
  )

  parsed = parseBadExplanationResponse(rawResponse)

  validated = validateBadExplanationResponse(
    parsed = parsed,
    plan = plan
  )

  formatBadExplanationOutput(
    parsed = validated,
    labelErrors = labelErrors
  )
}
