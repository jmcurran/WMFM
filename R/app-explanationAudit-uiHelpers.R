#' Render the explanation-audit panel for the app
#'
#' Creates a student-facing note explaining that the app now shows a teaching
#' summary rather than the raw audit structure directly.
#'
#' @param audit A `wmfmExplanationAudit` object, or `NULL`.
#'
#' @return A Shiny UI object.
#' @keywords internal
#' @importFrom htmltools tagList tags
renderModelExplanationAuditUi = function(audit) {

  if (is.null(audit)) {
    return(NULL)
  }

  tagList(
    tags$p(
      "The raw explanation audit is still kept for internal QA and testing, but the student-facing app now shows a teaching summary instead of exposing these internal details directly."
    )
  )
}

#' Convert backticked names into code-style chips for UI display
#'
#' @param text A single character string.
#'
#' @return A Shiny UI paragraph tag.
#' @keywords internal
#' @importFrom htmltools HTML htmlEscape
renderTeachingSummaryText = function(text) {

  text = as.character(text %||% "")

  if (!grepl("`", text, fixed = TRUE)) {
    return(tags$p(text))
  }

  parts = strsplit(text, "`", fixed = TRUE)[[1]]
  nodes = vector("list", length(parts))

  for (i in seq_along(parts)) {
    part = parts[[i]]

    if (i %% 2 == 0) {
      nodes[[i]] = tags$code(
        style = paste(
          "background-color: #f1f3f5;",
          "border: 1px solid #d0d7de;",
          "border-radius: 0.35rem;",
          "padding: 0.1rem 0.35rem;",
          "font-size: 0.95em;"
        ),
        part
      )
    } else if (nzchar(part)) {
      nodes[[i]] = htmltools::HTML(htmltools::htmlEscape(part))
    } else {
      nodes[[i]] = ""
    }
  }

  do.call(tags$p, nodes)
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
    renderTeachingSummaryText(text %||% "")
  }

  makeEvidenceList = function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) {
      return(tags$p("No extra teaching notes were needed for this explanation."))
    }

    items = lapply(seq_len(nrow(df)), function(i) {
      tags$li(
        tags$strong(paste0(df$section[[i]], ": ")),
        df$summary[[i]]
      )
    })

    do.call(tags$ul, items)
  }

  do.call(
    accordion,
    list(
      id = "model_explanation_teaching_summary",
      multiple = TRUE,
      open = FALSE,
      accordion_panel(
        title = "Scale used for the explanation",
        makeParagraph(summary$interpretationScale)
      ),
      accordion_panel(
        title = "Starting values and comparison groups",
        makeParagraph(summary$baselineChoice)
      ),
      accordion_panel(
        title = "What change is being described",
        makeParagraph(summary$xChangeDescription)
      ),
      accordion_panel(
        title = "How the main result was described",
        makeParagraph(summary$mainEffectDescription)
      ),
      accordion_panel(
        title = "How uncertainty was described",
        makeParagraph(summary$uncertaintySummary)
      ),
      accordion_panel(
        title = "Main pieces of information used",
        makeEvidenceList(summary$evidenceTable)
      ),
      accordion_panel(
        title = "Connection to the research question",
        makeParagraph(summary$researchQuestionLink)
      )
    )
  )
}
