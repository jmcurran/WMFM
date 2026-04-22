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
    return(supportNotes)
  }

  claimTags = row$claimTags[[1]] %||% character(0)
  claimTags = unique(stats::na.omit(as.character(claimTags)))

  if (length(claimTags) > 0) {
    return(buildExplanationClaimSupportNotes(
      claimTags = claimTags,
      matchedEvidence = data.frame(stringsAsFactors = FALSE)
    ))
  }

  supportNote = trimws(as.character(row$supportNote[[1]] %||% ""))

  if (nzchar(supportNote)) {
    return(supportNote)
  }

  character(0)
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

#' Render one explanation claim-evidence card
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#'
#' @return A Shiny UI object.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tagList tags
renderExplanationClaimEvidenceCardUi = function(row) {

  tags$div(
    class = "wmfm-explanation-helper-box",
    tags$p(
      class = "wmfm-explanation-helper-note",
      tags$strong(paste0("Sentence ", row$sentenceIndex[[1]], ": ")),
      row$claimText[[1]]
    ),
    renderExplanationClaimRoleNotesUi(row),
    renderExplanationClaimEvidenceLabelsUi(row)
  )
}
