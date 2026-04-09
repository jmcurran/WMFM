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
