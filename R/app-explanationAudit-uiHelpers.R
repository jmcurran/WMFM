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
      "The raw explanation audit is still available for internal QA and testing, but the student-facing app now uses the teaching summary instead of showing these internals directly."
    )
  )
}

#' Build a styled inline code chip
#'
#' @param text Character string to show inside the chip.
#'
#' @return A Shiny tag.
#' @keywords internal
makeExplanationTeachingCodeChip = function(text) {

  tags$code(
    style = paste(
      "font-family: SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace;",
      "background-color: #f1f3f5;",
      "border: 1px solid #d9dee3;",
      "border-radius: 4px;",
      "padding: 0.12em 0.35em;",
      "color: #495057;"
    ),
    text
  )
}

#' Convert backtick-marked teaching text into HTML tags
#'
#' @param text Character scalar using backticks around variable names.
#'
#' @return A tag list.
#' @keywords internal
renderExplanationTeachingInlineText = function(text) {

  text = text %||% ""
  parts = strsplit(text, "`", fixed = TRUE)[[1]]

  children = lapply(seq_along(parts), function(i) {
    part = parts[[i]]

    if (i %% 2 == 0) {
      return(makeExplanationTeachingCodeChip(part))
    }

    part
  })

  do.call(tagList, children)
}

#' Build a paragraph for teaching-summary text
#'
#' @param text Character scalar using backticks around variable names.
#'
#' @return A Shiny tag.
#' @keywords internal
makeExplanationTeachingParagraph = function(text) {

  tags$p(renderExplanationTeachingInlineText(text))
}

#' Build a student-facing evidence list
#'
#' @param df A data frame with `section` and `summary` columns.
#'
#' @return A Shiny tag.
#' @keywords internal
makeExplanationTeachingEvidenceList = function(df) {

  if (!is.data.frame(df) || nrow(df) == 0) {
    return(tags$p("No extra details were needed for this explanation."))
  }

  tags$ul(
    style = "margin-bottom: 0; padding-left: 1.2rem;",
    lapply(seq_len(nrow(df)), function(i) {
      tags$li(
        tags$strong(df$section[[i]]),
        ": ",
        renderExplanationTeachingInlineText(df$summary[[i]])
      )
    })
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

  tagList(
    tags$p(
      "This panel gives a short teaching summary of the choices the app made when it turned the fitted model into words."
    ),
    do.call(
      accordion,
      list(
        id = "model_explanation_teaching_summary",
        multiple = TRUE,
        open = FALSE,
        accordion_panel(
          title = "How the outcome was described",
          makeExplanationTeachingParagraph(summary$interpretationScale)
        ),
        accordion_panel(
          title = "What starting point was used",
          makeExplanationTeachingParagraph(summary$baselineChoice)
        ),
        accordion_panel(
          title = "What change was being described",
          makeExplanationTeachingParagraph(summary$xChangeDescription)
        ),
        accordion_panel(
          title = "How the main result was explained",
          makeExplanationTeachingParagraph(summary$mainEffectDescription)
        ),
        accordion_panel(
          title = "How uncertainty was shown",
          makeExplanationTeachingParagraph(summary$uncertaintySummary)
        ),
        accordion_panel(
          title = "What information the app used",
          makeExplanationTeachingEvidenceList(summary$evidenceTable)
        ),
        accordion_panel(
          title = "How this links to the research question",
          makeExplanationTeachingParagraph(summary$researchQuestionLink)
        )
      )
    )
  )
}
