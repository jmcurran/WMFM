#' Build the system prompt for WMFM explanation scoring
#'
#' Creates the fixed system prompt used when asking a language model to score a
#' plain-language model explanation against the WMFM scoring schema.
#'
#' @return A character scalar containing the system prompt.
#' @keywords internal
buildWmfmLlmScoringSystemPrompt = function() {

  paste(
    "You are scoring a plain-language explanation of a fitted statistical model.",
    "You must score the explanation against the supplied fitted-model context.",
    "Be strict, consistent, and conservative.",
    "",
    "Scoring rules:",
    "- Distinguish clearly between descriptive wording and inferential wording.",
    "- Penalise incorrect effect direction, incorrect effect scale, incorrect baseline/reference handling, and incorrect interaction interpretation.",
    "- Penalise causal language unless the supplied model context explicitly justifies causation.",
    "- If interaction terms are present, assess whether the explanation covers them adequately and correctly.",
    "- If interaction terms are absent, penalise invented interaction claims.",
    "- Penalise overclaiming more heavily than cautious wording.",
    "- Use only the information provided in the prompt.",
    "",
    "Output rules:",
    "- Return strict JSON only.",
    "- Do not wrap the JSON in markdown unless the provider forces it.",
    "- Use the exact field names requested.",
    "- For rubric fields scored on the 0/1/2 scale:",
    "  2 = correct, appropriate, adequate, or clear",
    "  1 = partly correct, incomplete, mixed, or somewhat unclear",
    "  0 = clearly wrong, inappropriate, missing when important, or seriously unclear",
    "",
    "Aggregate score rules:",
    "- factualScore, inferenceScore, completenessScore, clarityScore, calibrationScore, and overallScore must each be numeric values between 0 and 2 inclusive.",
    "- overallPass should usually be FALSE if fatalFlawDetected is TRUE.",
    sep = "\n"
  )
}
