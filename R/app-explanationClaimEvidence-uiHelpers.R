#' Render the claim-to-evidence map for the app
#'
#' Creates a student-facing display that shows each explanation sentence beside a
#' plain-language note about the deterministic evidence that supports it.
#'
#' @param claimMap A `wmfmExplanationClaimEvidenceMap` object, or `NULL`.
#'
#' @return A Shiny UI object.
#' @keywords internal
#' @importFrom htmltools tagList tags
renderExplanationClaimEvidenceUi = function(claimMap) {

  if (is.null(claimMap)) {
    return(NULL)
  }

  if (!is.data.frame(claimMap$claims) || nrow(claimMap$claims) == 0) {
    return(tags$p("No sentence-level explanation map is available for this explanation."))
  }

  cards = lapply(seq_len(nrow(claimMap$claims)), function(i) {
    row = claimMap$claims[i, , drop = FALSE]

    tags$div(
      class = "wmfm-explanation-helper-box",
      tags$p(
        class = "wmfm-explanation-helper-note",
        tags$strong(paste0("Sentence ", row$sentenceIndex[[1]], ": ")),
        row$claimText[[1]]
      ),
      tags$p(row$supportNote[[1]]),
      if (nzchar(row$evidenceLabels[[1]] %||% "")) {
        tags$p(
          class = "wmfm-explanation-helper-note",
          tags$strong("Evidence used: "),
          row$evidenceLabels[[1]]
        )
      }
    )
  })

  do.call(tagList, cards)
}
