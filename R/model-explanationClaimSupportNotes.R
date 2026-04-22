#' Get the plain-language note text for one explanation claim tag
#'
#' @param claimTag Single claim-tag string.
#'
#' @return A single character string.
#' @keywords internal
#' @noRd
getExplanationClaimSupportNoteForTag = function(claimTag) {

  noteRegistry = c(
    researchQuestion = "restates the research question",
    typicalCase = "describes a typical case",
    effect = "explains how the response changes",
    uncertainty = "shows uncertainty in the estimate",
    comparison = "describes how groups are being compared",
    answer = "helps answer the research question",
    scale = "explains the response scale"
  )

  noteKey = as.character(claimTag %||% "")[[1]]

  if (!nzchar(noteKey)) {
    return("provides supporting context")
  }

  note = unname(noteRegistry[noteKey])

  if (length(note) != 1 || is.na(note) || !nzchar(note)) {
    return("provides supporting context")
  }

  note
}

#' Build plain-language support notes for a mapped claim
#'
#' @param claimTags Character vector.
#' @param matchedEvidence Data frame of evidence rows.
#'
#' @return Character vector.
#' @keywords internal
buildExplanationClaimSupportNotes = function(claimTags, matchedEvidence) {

  tags = normaliseExplanationClaimTags(claimTags)

  if (length(tags) == 0) {
    return("provides supporting context")
  }

  notes = vapply(tags, function(tag) {
    getExplanationClaimSupportNoteForTag(tag)
  }, character(1))

  notes = unique(stats::na.omit(as.character(notes)))
  notes[nzchar(notes)]
}

#' Build a combined plain-language support note for a mapped claim
#'
#' @param claimTags Character vector.
#' @param matchedEvidence Data frame of evidence rows.
#' @param supportNotes Optional character vector.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationClaimSupportNote = function(claimTags, matchedEvidence, supportNotes = NULL) {

  notes = supportNotes %||% buildExplanationClaimSupportNotes(
    claimTags = claimTags,
    matchedEvidence = matchedEvidence
  )

  notes = unique(stats::na.omit(as.character(notes %||% character(0))))
  notes = notes[nzchar(notes)]

  if (length(notes) == 0) {
    return("This sentence provides supporting context for the explanation.")
  }

  pieces = c(
    paste0("This sentence ", notes[[1]], "."),
    if (length(notes) > 1) {
      paste0("It also ", notes[-1], ".")
    }
  )

  paste(pieces, collapse = " ")
}
