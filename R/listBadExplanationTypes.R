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
