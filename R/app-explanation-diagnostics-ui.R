#' Build Developer Mode explanation prompt diagnostics UI
#'
#' @param diagnostics Optional diagnostics list from model fitting.
#'
#' @return Shiny tag list.
#' @keywords internal
#' @noRd
buildExplanationPromptDiagnosticsUi = function(diagnostics = NULL) {
  if (is.null(diagnostics)) {
    return(tags$p(
      class = "wmfm-explanation-helper-note",
      "No explanation prompt diagnostics are available yet. Fit a model first."
    ))
  }

  payload = diagnostics$followupPayload %||% list()
  prediction = payload$predictionResult %||% list()
  missingOrAmbiguous = prediction$warnings %||% ""
  diagnosticsBundle = paste(
    "Raw follow-up question:", payload$originalText %||% diagnostics$followupText %||% "",
    "",
    "Follow-up category:", as.character(payload$category %||% ""),
    "",
    "Deterministic status:", as.character(prediction$status %||% "not_applicable"),
    "",
    "Resolved predictor values:", paste(capture.output(str(prediction$resolvedPredictorValues %||% list())), collapse = "\n"),
    "",
    "Prediction payload:", paste(capture.output(str(prediction)), collapse = "\n"),
    "",
    "Assembled prompt excerpt:", substr(diagnostics$assembledPrompt %||% "", 1, 8000),
    sep = "\n"
  )

  tagList(
    tags$strong("Explanation prompt diagnostics"),
    tags$p(
      class = "wmfm-explanation-helper-note",
      "Developer-mode diagnostics for follow-up classification, deterministic prediction payload, and assembled explanation prompt. Copy the diagnostics bundle into a debugging request when follow-up answers look wrong."
    ),
    tags$strong("Copyable diagnostics bundle"),
    tags$textarea(
      id = "diag_followup_bundle",
      readonly = "readonly",
      rows = 18,
      style = "width: 100%; font-family: monospace; white-space: pre;",
      diagnosticsBundle
    ),
    tags$strong("Raw follow-up question text received by server"),
    tags$pre(id = "diag_followup_raw_text", payload$originalText %||% diagnostics$followupText %||% ""),
    tags$strong("Follow-up classification"),
    tags$pre(id = "diag_followup_classification", as.character(payload$category %||% "")),
    tags$strong("Deterministic follow-up status"),
    tags$pre(id = "diag_followup_deterministic_status", as.character(prediction$status %||% "not_applicable")),
    tags$strong("Resolved predictor values"),
    tags$pre(id = "diag_followup_resolved_values", paste(capture.output(str(prediction$resolvedPredictorValues %||% list())), collapse = "\n")),
    tags$strong("Missing or ambiguous predictor values"),
    tags$pre(id = "diag_followup_missing_values", paste(capture.output(str(missingOrAmbiguous)), collapse = "\n")),
    tags$strong("Deterministic prediction payload"),
    tags$pre(id = "diag_followup_prediction_payload", paste(capture.output(str(prediction)), collapse = "\n")),
    tags$strong("Final assembled prompt excerpt"),
    tags$pre(id = "diag_followup_prompt_excerpt", substr(diagnostics$assembledPrompt %||% "", 1, 8000))
  )
}
