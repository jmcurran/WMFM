#' List supported bad explanation types
#'
#' Returns the currently supported error types that can be requested when
#' generating deliberately flawed model explanations.
#'
#' @return A character vector of supported bad explanation types.
#' @export
listBadExplanationTypes = function() {

  c(
    "nullAlternativeConfusion",
    "nullAcceptanceError",
    "causalInferenceError",
    "effectDirectionError",
    "wrongScaleError",
    "referenceLevelError",
    "factorOffsetOmissionError",
    "interactionIgnoredError",
    "mainEffectOverinterpretationWithInteraction",
    "confidenceIntervalProofLanguage",
    "rSquaredOverclaim",
    "inferenceOmission",
    "logicalContradiction"
  )
}

#' Get bad explanation types compatible with a WMFM model
#'
#' @param x A `wmfmModel` object.
#'
#' @return A character vector of compatible bad explanation types.
#' @keywords internal
getAvailableBadExplanationTypes = function(x) {

  compatibleTypes = c(
    "nullAlternativeConfusion",
    "nullAcceptanceError",
    "causalInferenceError",
    "effectDirectionError",
    "wrongScaleError",
    "confidenceIntervalProofLanguage",
    "rSquaredOverclaim",
    "inferenceOmission",
    "logicalContradiction"
  )

  factorPredictors = tryCatch(
    getFactorPredictors(x$model, x$data),
    error = function(e) {
      character(0)
    }
  )

  if (length(factorPredictors) > 0L) {
    compatibleTypes = c(
      compatibleTypes,
      "referenceLevelError",
      "factorOffsetOmissionError"
    )
  }

  interactionTerms = x$interactionTerms %||% character(0)
  interactionTerms = interactionTerms[!is.na(interactionTerms) & nzchar(interactionTerms)]

  if (length(interactionTerms) > 0L) {
    compatibleTypes = c(
      compatibleTypes,
      "interactionIgnoredError",
      "mainEffectOverinterpretationWithInteraction"
    )
  }

  unique(compatibleTypes)
}

#' Build textual definitions for supported bad explanation types
#'
#' @return A character scalar.
#' @keywords internal
buildBadExplanationTypeGuide = function() {

  paste(
    "- nullAlternativeConfusion: reject the null hypothesis and state that the alternative hypothesis is true or accepted.",
    "- nullAcceptanceError: say that the null hypothesis is accepted or proven true.",
    "- causalInferenceError: describe an association as if it were causal.",
    "- effectDirectionError: reverse the direction of an effect.",
    "- wrongScaleError: confuse additive change with percentage or relative change.",
    "- referenceLevelError: misidentify the reference level or comparison group.",
    "- factorOffsetOmissionError: treat a factor offset as if it were the full fitted value.",
    "- interactionIgnoredError: ignore a meaningful interaction and explain only main effects.",
    "- mainEffectOverinterpretationWithInteraction: interpret a main effect as generally true despite an interaction.",
    "- confidenceIntervalProofLanguage: treat confidence intervals as proof or guaranteed truth.",
    "- rSquaredOverclaim: overstate model fit, explanatory power, or predictive accuracy.",
    "- inferenceOmission: remove meaningful inferential interpretation while keeping some descriptive content.",
    "- logicalContradiction: include a contradiction within the explanation.",
    sep = "\n"
  )
}

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
