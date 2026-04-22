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
    renderExplanationClaimEvidenceCardUi(row)
  })

  nSentences = length(cards)

  guideText = if (nSentences == 1) {
    "There is 1 sentence in this explanation. A sentence can play more than one role."
  } else {
    paste0(
      "There are ",
      nSentences,
      " sentences in this explanation. A sentence can play more than one role."
    )
  }

  do.call(tagList, c(
    list(
      tags$p(
        class = "wmfm-explanation-helper-note",
        guideText
      )
    ),
    cards
  ))
}
