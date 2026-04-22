#' Build downstream support notes for a deterministic explanation audit
#'
#' Summarises how later rebuild streams can safely use each top-level section of
#' a `wmfmExplanationAudit` object. This keeps the handoff guidance close to the
#' code and provides a deterministic reference for later teaching-summary,
#' claim-evidence, and UI work.
#'
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return A data frame describing section purpose and downstream usage.
#' @keywords internal
buildWmfmExplanationAuditSupportNotes = function(audit) {

  validateWmfmExplanationAudit(x = audit)

  data.frame(
    section = c(
      "overview",
      "promptInputs",
      "promptRules",
      "interpretationScale",
      "numericAnchor",
      "referenceLevels",
      "confidenceIntervals",
      "baselineEvidence",
      "effectEvidence",
      "coefficientTable",
      "rawPromptIngredients"
    ),
    purpose = c(
      "Model metadata and availability flags for later consumers.",
      "Records deterministic inputs passed into explanation construction.",
      "Summarises durable language-side rules without exposing hidden reasoning.",
      "Declares the student-facing scale used for fitted values and effects.",
      "Records the numeric reference rule and anchor table for numeric predictors.",
      "Records baseline groups for factor predictors.",
      "Summarises confidence-interval availability and displayed scales.",
      "Stores deterministic baseline evidence rows when they are available.",
      "Stores deterministic effect and contrast evidence rows when they are available.",
      "Stores rounded coefficient summaries for developer inspection and QA.",
      "Stores raw prompt ingredients for internal QA only, not direct student display."
    ),
    downstreamUse = c(
      "Safe for teaching-summary and UI headings.",
      "Safe for QA and selective student-facing summaries.",
      "Use for QA or curated teaching notes, not raw display.",
      "Safe for teaching-summary and explanation framing.",
      "Safe for teaching-summary baseline explanations and UI labels.",
      "Safe for teaching-summary baseline explanations and UI labels.",
      "Safe for teaching-summary uncertainty summaries.",
      "Use when available; later streams must tolerate zero rows.",
      "Use when available; later streams must tolerate zero rows.",
      "Use for QA and optional developer display.",
      "Keep internal; do not expose directly in the student-facing app."
    ),
    stringsAsFactors = FALSE
  )
}
