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
renderModelExplanationAuditUi = function(audit) {

  if (is.null(audit)) {
    return(NULL)
  }

  tagList(
    tags$p(
      "The raw explanation audit is available for internal QA and testing, but the student-facing app now uses the teaching summary instead of exposing these details directly."
    )
  )
}

#' Render the explanation teaching summary for the app
#'
#' Creates a student-facing accordion that explains, in plain language, how the
#' explanation was constructed from the deterministic audit.
#'
#' @param summary A `wmfmExplanationTeachingSummary` object, or `NULL`.
#'
#' @return A Shiny UI object.
#' @keywords internal
#' @importFrom bslib accordion accordion_panel
#' @importFrom htmltools tagList tags
renderExplanationTeachingSummaryUi = function(summary) {

  if (is.null(summary)) {
    return(NULL)
  }

  makeParagraph = function(text) {
    tags$p(text %||% "")
  }

  makeEvidenceTable = function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(tags$p("No extra evidence details were needed for this explanation."))
    }

    header = tags$tr(
      lapply(names(df), function(name) {
        tags$th(name)
      })
    )

    body = lapply(seq_len(nrow(df)), function(i) {
      tags$tr(
        lapply(df[i, , drop = FALSE], function(value) {
          tags$td(as.character(value))
        })
      )
    })

    tags$table(
      class = "table table-sm table-striped",
      tags$thead(header),
      tags$tbody(body)
    )
  }

  do.call(
    accordion,
    list(
      id = "model_explanation_teaching_summary",
      multiple = TRUE,
      open = FALSE,
      accordion_panel(
        title = "Interpretation scale",
        makeParagraph(summary$interpretationScale)
      ),
      accordion_panel(
        title = "Reference and baseline choices",
        makeParagraph(summary$baselineChoice)
      ),
      accordion_panel(
        title = "What change is being interpreted",
        makeParagraph(summary$xChangeDescription)
      ),
      accordion_panel(
        title = "Main effect interpretation",
        makeParagraph(summary$mainEffectDescription)
      ),
      accordion_panel(
        title = "How uncertainty was handled",
        makeParagraph(summary$uncertaintySummary)
      ),
      accordion_panel(
        title = "Summary of evidence used",
        makeEvidenceTable(summary$evidenceTable)
      ),
      accordion_panel(
        title = "Connection to the research question",
        makeParagraph(summary$researchQuestionLink)
      )
    )
  )
}
