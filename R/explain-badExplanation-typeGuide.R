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
