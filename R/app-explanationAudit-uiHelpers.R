#' Render the explanation-audit panel for the app
#'
#' Creates a student-facing accordion that explains how the model explanation
#' was constructed from deterministic inputs and model-derived evidence.
#'
#' @param audit A `wmfmExplanationAudit` object, or `NULL`.
#'
#' @return A Shiny UI object.
#' @keywords internal
#' @importFrom bslib accordion accordion_panel
#' @importFrom htmltools tagList tags
#' @importFrom shiny tableOutput
renderModelExplanationAuditUi = function(audit) {

  if (is.null(audit)) {
    return(NULL)
  }

  makeKv = function(label, value) {
    tags$div(
      tags$strong(paste0(label, ": ")),
      as.character(value)
    )
  }

  numericAnchorItems = list()
  if (nzchar(audit$numericAnchor$note %||% "")) {
    numericAnchorItems[[length(numericAnchorItems) + 1]] = tags$p(audit$numericAnchor$note)
  }
  if (is.data.frame(audit$numericAnchor$table) && nrow(audit$numericAnchor$table) > 0) {
    numericAnchorItems[[length(numericAnchorItems) + 1]] = tags$h5("Numeric predictors")
    numericAnchorItems[[length(numericAnchorItems) + 1]] = tableOutput("model_explanation_audit_numeric_anchor")
  }
  if (is.data.frame(audit$referenceLevels) && nrow(audit$referenceLevels) > 0) {
    numericAnchorItems[[length(numericAnchorItems) + 1]] = tags$h5("Factor reference levels")
    numericAnchorItems[[length(numericAnchorItems) + 1]] = tableOutput("model_explanation_audit_reference_levels")
  }

  ciEvidenceItems = list(
    makeKv("Confidence level", "95%"),
    makeKv("CI summary mode", audit$confidenceIntervals$mode %||% "")
  )
  if (length(audit$confidenceIntervals$displayedScales %||% character(0)) > 0) {
    ciEvidenceItems[[length(ciEvidenceItems) + 1]] = makeKv(
      "Displayed scales",
      paste(audit$confidenceIntervals$displayedScales, collapse = ", ")
    )
  }
  if (nzchar(audit$confidenceIntervals$note %||% "")) {
    ciEvidenceItems[[length(ciEvidenceItems) + 1]] = tags$p(audit$confidenceIntervals$note)
  }
  if (is.data.frame(audit$baselineEvidence) && nrow(audit$baselineEvidence) > 0) {
    ciEvidenceItems[[length(ciEvidenceItems) + 1]] = tags$h5("Baseline quantities")
    ciEvidenceItems[[length(ciEvidenceItems) + 1]] = tableOutput("model_explanation_audit_baseline")
  }
  if (is.data.frame(audit$effectEvidence) && nrow(audit$effectEvidence) > 0) {
    ciEvidenceItems[[length(ciEvidenceItems) + 1]] = tags$h5("Effect quantities")
    ciEvidenceItems[[length(ciEvidenceItems) + 1]] = tableOutput("model_explanation_audit_effects")
  }

  promptItems = list(
    makeKv("Response", audit$promptInputs$response %||% ""),
    makeKv("Response description", audit$promptInputs$responseNounPhrase %||% ""),
    makeKv("Predictors", paste(audit$promptInputs$predictors %||% character(0), collapse = ", ")),
    makeKv("Dataset context used", if (isTRUE(audit$promptInputs$datasetContextUsed)) "Yes" else "No"),
    makeKv("Research question used", if (isTRUE(audit$promptInputs$researchQuestionUsed)) "Yes" else "No")
  )
  if (isTRUE(audit$promptInputs$researchQuestionUsed)) {
    promptItems[[length(promptItems) + 1]] = makeKv(
      "Research question",
      audit$promptInputs$researchQuestion %||% ""
    )
  }

  panels = list(
    accordion_panel(
      title = "What this panel is showing",
      do.call(tagList, list(tags$p(audit$transparencyNote)))
    ),
    accordion_panel(
      title = "Prompt ingredients",
      do.call(tagList, promptItems)
    ),
    accordion_panel(
      title = "Interpretation rules applied",
      tags$ul(
        lapply(audit$promptRules %||% character(0), tags$li)
      )
    ),
    accordion_panel(
      title = "Interpretation scale and back-transformation",
      tagList(
        makeKv("Response expression", audit$interpretationScale$responseExpression %||% ""),
        makeKv("Fitted-value scale", audit$interpretationScale$fittedValueScale %||% ""),
        makeKv("Effect scale", audit$interpretationScale$effectScale %||% ""),
        makeKv("Back-transformation", audit$interpretationScale$backTransformation %||% ""),
        tags$p(audit$interpretationScale$explanationScaleNote %||% "")
      )
    ),
    accordion_panel(
      title = "Numeric anchors and reference levels",
      do.call(tagList, numericAnchorItems)
    ),
    accordion_panel(
      title = "Confidence-interval evidence used",
      do.call(tagList, ciEvidenceItems)
    )
  )

  do.call(
    accordion,
    c(
      list(
        id = "model_explanation_audit",
        multiple = TRUE,
        open = FALSE
      ),
      panels
    )
  )
}
