#' Build student-facing role notes for a claim-evidence UI row
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
buildExplanationClaimUiRoleNotes = function(row) {

  supportNotes = row$supportNotes[[1]] %||% character(0)
  supportNotes = unique(stats::na.omit(as.character(supportNotes)))

  if (length(supportNotes) > 0) {
    return(orderExplanationClaimUiRoleNotes(
      roleNotes = supportNotes,
      claimTags = row$claimTags[[1]] %||% character(0)
    ))
  }

  claimTags = row$claimTags[[1]] %||% character(0)
  claimTags = unique(stats::na.omit(as.character(claimTags)))

  if (length(claimTags) > 0) {
    return(orderExplanationClaimUiRoleNotes(
      roleNotes = buildExplanationClaimSupportNotes(
        claimTags = claimTags,
        matchedEvidence = data.frame(stringsAsFactors = FALSE)
      ),
      claimTags = claimTags
    ))
  }

  supportNote = trimws(as.character(row$supportNote[[1]] %||% ""))

  if (nzchar(supportNote)) {
    return(supportNote)
  }

  character(0)
}

#' Order student-facing role notes for clearer display
#'
#' @param roleNotes Character vector.
#' @param claimTags Character vector.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
orderExplanationClaimUiRoleNotes = function(roleNotes, claimTags = character(0)) {

  notes = unique(stats::na.omit(as.character(roleNotes %||% character(0))))
  notes = notes[nzchar(notes)]

  if (length(notes) <= 1) {
    return(notes)
  }

  tags = unique(stats::na.omit(as.character(claimTags %||% character(0))))

  if (!("answer" %in% tags)) {
    return(notes)
  }

  answerPattern = "answer the research question"
  answerNotes = notes[grepl(answerPattern, notes, fixed = TRUE)]
  otherNotes = notes[!grepl(answerPattern, notes, fixed = TRUE)]

  c(answerNotes, otherNotes)
}

#' Render student-facing role notes for one claim-evidence row
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#'
#' @return A Shiny UI object.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tagList tags
renderExplanationClaimRoleNotesUi = function(row) {

  roleNotes = buildExplanationClaimUiRoleNotes(row)

  if (length(roleNotes) == 0) {
    return(NULL)
  }

  tagList(
    tags$p(
      class = "wmfm-explanation-helper-note",
      tags$strong("What this sentence is doing:")
    ),
    tags$ul(
      class = "wmfm-explanation-helper-note",
      lapply(roleNotes, function(note) {
        tags$li(note)
      })
    )
  )
}

#' Build a student-facing gloss for one evidence label
#'
#' @param label Character scalar.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
buildExplanationEvidenceLabelGloss = function(label) {

  label = trimws(as.character(label %||% ""))

  if (!nzchar(label)) {
    return("")
  }

  if (identical(label, "Research question framing")) {
    return("the question the model is trying to answer")
  }

  if (label %in% c("Interpretation scale", "Model interpretation scale")) {
    return("the scale used to describe the result")
  }

  if (identical(label, "Starting values and comparison groups")) {
    return("the starting values or reference groups used in the explanation")
  }

  if (identical(label, "Main effect translation")) {
    return("the main fitted result rewritten in plain language")
  }

  if (label %in% c("Uncertainty summary", "Confidence interval rule")) {
    return("the uncertainty rule used to keep the wording cautious")
  }

  if (
    identical(label, "Reference level") ||
    grepl("^Reference level for `.+`$", label)
  ) {
    return("the comparison group used as the starting point")
  }

  if (grepl("^Starting value for `.+`$", label)) {
    return("the chosen starting value for this predictor")
  }

  if (grepl("^Baseline", label)) {
    return("the fitted starting value used for a typical case")
  }

  if (grepl("^Effect", label)) {
    return("the fitted effect quantity used for this part of the explanation")
  }

  ""
}

#' Render the evidence label text for one claim-evidence row
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tagList tags
renderExplanationClaimEvidenceLabelsUi = function(row) {

  evidenceLabels = trimws(as.character(row$evidenceLabels[[1]] %||% ""))

  if (!nzchar(evidenceLabels)) {
    return(NULL)
  }

  labelParts = trimws(strsplit(evidenceLabels, "\\|", fixed = FALSE)[[1]])
  labelParts = labelParts[nzchar(labelParts)]

  if (length(labelParts) == 0) {
    return(NULL)
  }

  tagList(
    tags$p(
      class = "wmfm-explanation-helper-note",
      tags$strong("Main model information behind it:")
    ),
    tags$ul(
      class = "wmfm-explanation-helper-note",
      lapply(labelParts, function(label) {
        gloss = buildExplanationEvidenceLabelGloss(label)

        tags$li(
          if (nzchar(gloss)) {
            tagList(tags$strong(paste0(label, ": ")), gloss)
          } else {
            label
          }
        )
      })
    )
  )
}

#' Sanitize a sentence index for developer feedback input ids
#'
#' @param sentenceIndex Sentence index.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
sanitizeDeveloperFeedbackSentenceIndex = function(sentenceIndex) {

  indexText = as.character(sentenceIndex %||% "")
  gsub("[^A-Za-z0-9_]+", "_", indexText)
}

#' Build a stable input id for a developer feedback checkbox
#'
#' @param sentenceIndex Sentence index.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
buildDeveloperFeedbackIncorrectInputId = function(sentenceIndex) {

  paste0(
    "developerFeedbackIncorrect_",
    sanitizeDeveloperFeedbackSentenceIndex(sentenceIndex)
  )
}

#' Build a stable input id for a developer feedback comment box
#'
#' @param sentenceIndex Sentence index.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
buildDeveloperFeedbackCommentInputId = function(sentenceIndex) {

  paste0(
    "developerFeedbackComment_",
    sanitizeDeveloperFeedbackSentenceIndex(sentenceIndex)
  )
}

#' Normalize an optional list-column value for developer UI display
#'
#' @param x Value to normalize.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
normalizeExplanationDeveloperUiValues = function(x) {

  values = as.character(x %||% character(0))
  values = trimws(values)
  values[nzchar(values)]
}

#' Render one developer-only metadata row
#'
#' @param label Character scalar.
#' @param values Character vector.
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tags
renderExplanationDeveloperMetadataRowUi = function(label, values) {

  values = normalizeExplanationDeveloperUiValues(values)

  if (length(values) == 0) {
    return(NULL)
  }

  tags$li(
    tags$strong(paste0(label, ": ")),
    paste(values, collapse = ", ")
  )
}

#' Render developer-only sentence schema diagnostics
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#' @param developerMode Logical. Should developer-only diagnostics be displayed?
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tagList tags
renderExplanationDeveloperSchemaDiagnosticsUi = function(row, developerMode = FALSE) {

  if (!isTRUE(developerMode)) {
    return(NULL)
  }

  primaryRole = if ("primaryRole" %in% names(row)) {
    row$primaryRole[[1]]
  } else {
    character(0)
  }

  roles = if ("roles" %in% names(row)) {
    row$roles[[1]]
  } else {
    character(0)
  }

  qualityFlags = if ("qualityFlags" %in% names(row)) {
    row$qualityFlags[[1]]
  } else {
    character(0)
  }

  supportMapIds = if ("supportMapIds" %in% names(row)) {
    row$supportMapIds[[1]]
  } else {
    character(0)
  }

  rows = list(
    renderExplanationDeveloperMetadataRowUi("Primary role", primaryRole),
    renderExplanationDeveloperMetadataRowUi("Roles", roles),
    renderExplanationDeveloperMetadataRowUi("Quality flags", qualityFlags),
    renderExplanationDeveloperMetadataRowUi("Support map ids", supportMapIds)
  )

  rows = rows[!vapply(rows, is.null, logical(1))]

  if (length(rows) == 0) {
    return(NULL)
  }

  tagList(
    tags$p(
      class = "wmfm-explanation-helper-note",
      tags$strong("Developer diagnostics:")
    ),
    tags$ul(
      class = "wmfm-explanation-helper-note wmfm-developer-schema-diagnostics",
      rows
    )
  )
}

#' Render developer-only sentence feedback controls
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#' @param developerMode Logical. Should developer-only feedback controls be
#'   displayed?
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tags
#' @importFrom shiny checkboxInput conditionalPanel textAreaInput
renderExplanationDeveloperFeedbackControlsUi = function(row, developerMode = FALSE) {

  if (!isTRUE(developerMode)) {
    return(NULL)
  }

  sentenceIndex = row$sentenceIndex[[1]]
  incorrectInputId = buildDeveloperFeedbackIncorrectInputId(sentenceIndex)
  commentInputId = buildDeveloperFeedbackCommentInputId(sentenceIndex)

  tags$div(
    class = "wmfm-developer-feedback-controls",
    checkboxInput(
      inputId = incorrectInputId,
      label = "Mark as incorrect",
      value = FALSE
    ),
    conditionalPanel(
      condition = paste0("input['", incorrectInputId, "'] === true"),
      textAreaInput(
        inputId = commentInputId,
        label = "Describe the issue",
        value = "",
        rows = 3,
        width = "100%",
        placeholder = paste(
          "Explain what is wrong with this sentence or its support mapping.",
          "For example, note an incorrect scale, missing interaction,",
          "wrong baseline, unsupported claim, or unclear wording."
        )
      )
    )
  )
}
#' Render one explanation claim-evidence card
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#' @param developerMode Logical. Should developer-only feedback controls be
#'   displayed?
#'
#' @return A Shiny UI object.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tagList tags
renderExplanationClaimEvidenceCardUi = function(row, developerMode = FALSE) {

  displayClaimText = cleanExplanationText(row$claimText[[1]])

  tags$div(
    class = "wmfm-explanation-helper-box",
    tags$p(
      class = "wmfm-explanation-helper-note",
      tags$strong(paste0("Sentence ", row$sentenceIndex[[1]], ": ")),
      displayClaimText
    ),
    renderExplanationClaimRoleNotesUi(row),
    renderExplanationClaimEvidenceLabelsUi(row),
    renderExplanationDeveloperSchemaDiagnosticsUi(
      row = row,
      developerMode = isTRUE(developerMode)
    ),
    renderExplanationDeveloperFeedbackControlsUi(
      row = row,
      developerMode = isTRUE(developerMode)
    )
  )
}
