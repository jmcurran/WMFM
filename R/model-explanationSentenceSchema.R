#' Explanation sentence-role ontology
#'
#' These helpers define the Stage 9 multi-role sentence schema while preserving
#' the older `claimTags` and `claimType` fields used by the existing claim and
#' developer-feedback machinery.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
getExplanationSentenceRoleOntology = function() {

  c(
    "researchQuestion",
    "effect",
    "answer",
    "comparison",
    "uncertainty",
    "typicalCase",
    "evidence",
    "modelContext",
    "modelConstraint",
    "statisticalDisclaimer",
    "scaleTranslation"
  )
}

#' Explanation primary-role priority
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
getExplanationSentencePrimaryRolePriority = function() {

  c(
    "researchQuestion",
    "effect",
    "answer",
    "comparison",
    "typicalCase",
    "uncertainty",
    "evidence",
    "modelConstraint",
    "statisticalDisclaimer",
    "scaleTranslation",
    "modelContext"
  )
}

#' Normalize explanation sentence roles
#'
#' @param roles Character vector.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
normaliseExplanationSentenceRoles = function(roles) {

  aliases = c(
    researchQuestion = "researchQuestion",
    answer = "answer",
    effect = "effect",
    comparison = "comparison",
    uncertainty = "uncertainty",
    typicalCase = "typicalCase",
    evidence = "evidence",
    modelContext = "modelContext",
    modelConstraint = "modelConstraint",
    statisticalDisclaimer = "statisticalDisclaimer",
    scaleTranslation = "scaleTranslation",
    baseline = "typicalCase",
    mainEffect = "effect",
    scale = "scaleTranslation",
    context = "modelContext",
    disclaimer = "statisticalDisclaimer"
  )

  out = unique(stats::na.omit(as.character(roles %||% character(0))))
  out = trimws(out)
  out = out[nzchar(out)]

  out = vapply(out, function(role) {
    if (role %in% names(aliases)) {
      return(unname(aliases[[role]]))
    }

    role
  }, character(1), USE.NAMES = FALSE)

  out = unique(out)
  ontology = getExplanationSentenceRoleOntology()
  ontology[ontology %in% out]
}

#' Convert legacy claim tags to Stage 9 sentence roles
#'
#' @param claimTags Character vector.
#' @param matchedEvidence Optional evidence data frame.
#' @param claimText Optional claim text.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
mapExplanationClaimTagsToSentenceRoles = function(
    claimTags,
    matchedEvidence = NULL,
    claimText = NULL
) {

  roles = normaliseExplanationSentenceRoles(claimTags)

  if (sentenceHasEvidenceRole(
    claimText = claimText,
    matchedEvidence = matchedEvidence
  )) {
    roles = c(roles, "evidence")
  }

  normaliseExplanationSentenceRoles(roles)
}

#' Decide whether a sentence should carry the evidence role
#'
#' @param claimText Optional claim text.
#' @param matchedEvidence Optional evidence data frame.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
sentenceHasEvidenceRole = function(claimText = NULL, matchedEvidence = NULL) {

  text = trimws(as.character(claimText %||% ""))

  if (nzchar(text) && grepl("[0-9]", text, perl = TRUE)) {
    return(TRUE)
  }

  if (!is.data.frame(matchedEvidence) || nrow(matchedEvidence) == 0) {
    return(FALSE)
  }

  evidenceTypes = unique(stats::na.omit(as.character(matchedEvidence$evidenceType %||% character(0))))
  any(evidenceTypes %in% c(
    "baselineEvidence",
    "effectEvidence",
    "confidenceInterval",
    "numericAnchor"
  ))
}

#' Detect Stage 9 sentence roles without changing legacy claim tags
#'
#' @param claimText Character scalar.
#' @param claimTags Character vector of legacy claim tags.
#' @param matchedEvidence Optional evidence data frame.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
detectExplanationSentenceRoles = function(
    claimText,
    claimTags = character(0),
    matchedEvidence = NULL
) {

  roles = mapExplanationClaimTagsToSentenceRoles(
    claimTags = claimTags,
    matchedEvidence = matchedEvidence,
    claimText = claimText
  )

  roles = c(
    roles,
    if (detectModelContext(claimText = claimText)) "modelContext",
    if (detectModelConstraint(claimText = claimText)) "modelConstraint",
    if (detectStatisticalDisclaimer(claimText = claimText)) "statisticalDisclaimer"
  )

  normaliseExplanationSentenceRoles(roles)
}

#' Derive the primary role for an explanation sentence
#'
#' @param roles Character vector.
#'
#' @return Character scalar.
#' @keywords internal
#' @noRd
deriveExplanationPrimaryRole = function(roles) {

  roles = normaliseExplanationSentenceRoles(roles)

  if (length(roles) == 0) {
    return("modelContext")
  }

  priority = getExplanationSentencePrimaryRolePriority()

  for (role in priority) {
    if (role %in% roles) {
      return(role)
    }
  }

  roles[[1]]
}

#' Build support-map ids for a mapped sentence
#'
#' @param matchedEvidence Optional evidence data frame.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
buildExplanationSentenceSupportMapIds = function(matchedEvidence = NULL) {

  if (!is.data.frame(matchedEvidence) || nrow(matchedEvidence) == 0) {
    return(character(0))
  }

  ids = unique(stats::na.omit(as.character(matchedEvidence$evidenceId %||% character(0))))
  ids[nzchar(ids)]
}

#' Build deterministic quality flags for a mapped sentence
#'
#' Stage 9.5 only creates the schema hook. Later stages will add substantive
#' deterministic validators while preserving this field.
#'
#' @param claimText Character scalar.
#' @param roles Character vector.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
buildExplanationSentenceQualityFlags = function(claimText, roles) {

  buildExplanationDeterministicQualityFlags(
    claimText = claimText,
    roles = roles
  )
}
