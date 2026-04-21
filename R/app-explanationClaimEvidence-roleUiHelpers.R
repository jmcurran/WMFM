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
      tags$strong("This sentence:")
    ),
    tags$ul(
      class = "wmfm-explanation-helper-note",
      lapply(roleNotes, function(note) {
        tags$li(note)
      })
    )
  )
}

#' Render the evidence label text for one claim-evidence row
#'
#' @param row Single-row data frame from a
#'   `wmfmExplanationClaimEvidenceMap$claims` table.
#'
#' @return A Shiny UI object or `NULL`.
#' @keywords internal
#' @noRd
#' @importFrom htmltools tags
renderExplanationClaimEvidenceLabelsUi = function(row) {

  evidenceLabels = trimws(as.character(row$evidenceLabels[[1]] %||% ""))

  if (!nzchar(evidenceLabels)) {
    return(NULL)
  }

  tags$p(
    class = "wmfm-explanation-helper-note",
    tags$strong("Evidence used: "),
    evidenceLabels
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
