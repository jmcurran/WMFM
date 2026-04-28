#' Build a deterministic claim-to-evidence map for a model explanation
#'
#' Creates a student-safe, inspectable mapping from the final explanation text
#' back to the deterministic evidence used to support it. The mapping is built
#' from sentence-level units in the final explanation and links each sentence to
#' evidence categories drawn from the explanation audit and teaching summary.
#'
#' This object is intentionally not presented as hidden chain-of-thought. It is
#' a transparent post hoc map showing which deterministic ingredients best match
#' the visible explanation text.
#'
#' The current implementation uses a tag-first hybrid approach:
#' - deterministic sentence-level claim tags from the audit and teaching summary
#' - lightweight sentence-level evidence matching against the final explanation text
#' - a derived legacy single-label `claimType` field kept for compatibility
#'
#' @param explanationText Character scalar containing the final explanation.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional `wmfmExplanationTeachingSummary` object.
#'   When `NULL` and `model` is supplied, one is built automatically.
#' @param model Optional fitted model object, used to derive a teaching summary
#'   and to improve sentence matching.
#'
#' @return An object of class `wmfmExplanationClaimEvidenceMap`.
#' @export
#' @importFrom stats na.omit
buildExplanationClaimEvidenceMap = function(
    explanationText,
    audit,
    teachingSummary = NULL,
    model = NULL
) {

  if (is.null(audit)) {
    stop("`audit` must not be NULL.", call. = FALSE)
  }

  if (!is.character(explanationText) || length(explanationText) != 1 || is.na(explanationText)) {
    stop("`explanationText` must be a single non-missing character string.", call. = FALSE)
  }

  if (is.null(teachingSummary) && !is.null(model)) {
    teachingSummary = tryCatch(
      buildExplanationTeachingSummary(audit = audit, model = model),
      error = function(e) {
        NULL
      }
    )
  }

  explanationText = cleanExplanationText(explanationText)

  claimTexts = splitExplanationIntoClaimUnits(explanationText)
  evidenceInventory = buildExplanationEvidenceInventory(
    audit = audit,
    teachingSummary = teachingSummary,
    model = model
  )

  claims = lapply(seq_along(claimTexts), function(i) {
    mapSingleExplanationClaim(
      claimId = paste0("claim_", i),
      sentenceIndex = i,
      claimText = claimTexts[[i]],
      audit = audit,
      teachingSummary = teachingSummary,
      model = model,
      evidenceInventory = evidenceInventory,
      totalClaims = length(claimTexts),
      priorClaimTexts = if (i > 1L) claimTexts[seq_len(i - 1L)] else character(0)
    )
  })

  claimsTable = if (length(claims) == 0) {
    data.frame(
      claimId = character(0),
      sentenceIndex = integer(0),
      claimText = character(0),
      claimTags = I(list()),
      claimType = character(0),
      primaryRole = character(0),
      roles = I(list()),
      qualityFlags = I(list()),
      supportMapIds = I(list()),
      supportNotes = I(list()),
      supportNote = character(0),
      evidenceCount = integer(0),
      evidenceTypes = character(0),
      evidenceLabels = character(0),
      mappingMethod = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    do.call(rbind, claims)
  }

  claimsTable = applyStructuralAnswerSelection(
    claimsTable = claimsTable,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    evidenceInventory = evidenceInventory
  )

  out = list(
    transparencyNote = paste(
      "This map links visible explanation sentences to deterministic supporting evidence.",
      "It does not claim to reveal hidden chain-of-thought."
    ),
    mappingMethod = paste(
      "Sentence-level tag-first mapping using deterministic claim tags, the audit,",
      "the teaching summary, and lightweight text heuristics."
    ),
    unit = "sentence",
    evidenceInventory = evidenceInventory,
    claims = claimsTable
  )

  attr(out, "qualityFlags") = buildExplanationMapQualityFlags(claimsTable)
  class(out) = c("wmfmExplanationClaimEvidenceMap", class(out))
  out
}

#' Split an explanation into sentence-level claim units
#'
#' @param explanationText Character scalar.
#'
#' @return Character vector.
#' @keywords internal
splitExplanationIntoClaimUnits = function(explanationText) {

  text = gsub("[[:space:]]+", " ", trimws(explanationText %||% ""))

  if (!nzchar(text)) {
    return(character(0))
  }

  parts = strsplit(text, "(?<=[.!?])\\s+", perl = TRUE)[[1]]
  parts = trimws(parts)
  parts[nzchar(parts)]
}

#' Build a deterministic evidence inventory for explanation mapping
#'
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#'
#' @return A data frame.
#' @keywords internal
buildExplanationEvidenceInventory = function(audit, teachingSummary = NULL, model = NULL) {

  rows = list()

  addRow = function(evidenceType, sourceSection, label, summary) {
    rows[[length(rows) + 1]] <<- data.frame(
      evidenceId = paste0("evidence_", length(rows) + 1),
      evidenceType = evidenceType,
      sourceSection = sourceSection,
      label = label,
      summary = summary,
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(teachingSummary)) {
    researchText = trimws(as.character(teachingSummary$researchQuestionLink %||% ""))

    if (nzchar(researchText)) {
      addRow(
        evidenceType = "researchQuestion",
        sourceSection = "researchQuestionLink",
        label = "Research question framing",
        summary = researchText
      )
    }

    dataDescriptionText = trimws(as.character(teachingSummary$dataDescription %||% ""))

    if (nzchar(dataDescriptionText)) {
      addRow(
        evidenceType = "dataDescription",
        sourceSection = "dataDescription",
        label = "Data description",
        summary = dataDescriptionText
      )
    }

    addRow(
      evidenceType = "scale",
      sourceSection = "interpretationScale",
      label = "Interpretation scale",
      summary = teachingSummary$interpretationScale %||% ""
    )

    addRow(
      evidenceType = "baselineChoice",
      sourceSection = "baselineChoice",
      label = "Starting values and comparison groups",
      summary = teachingSummary$baselineChoice %||% ""
    )

    addRow(
      evidenceType = "mainEffect",
      sourceSection = "mainEffectDescription",
      label = "Main effect translation",
      summary = teachingSummary$mainEffectDescription %||% ""
    )

    addRow(
      evidenceType = "uncertainty",
      sourceSection = "uncertaintySummary",
      label = "Uncertainty summary",
      summary = teachingSummary$uncertaintySummary %||% ""
    )
  }

  if (is.list(audit$interpretationScale)) {
    addRow(
      evidenceType = "scale",
      sourceSection = "interpretationScale",
      label = "Model interpretation scale",
      summary = paste(
        audit$interpretationScale$fittedValueScale %||% "",
        audit$interpretationScale$effectScale %||% ""
      )
    )
  }

  if (is.list(audit$numericAnchor) && is.data.frame(audit$numericAnchor$table) && nrow(audit$numericAnchor$table) > 0) {
    for (i in seq_len(nrow(audit$numericAnchor$table))) {
      row = audit$numericAnchor$table[i, , drop = FALSE]
      addRow(
        evidenceType = "numericAnchor",
        sourceSection = "numericAnchor",
        label = paste0("Starting value for `", row$predictor[[1]], "`"),
        summary = paste(
          "Anchor:",
          as.character(row$anchor[[1]]),
          "Reason:",
          row$reason[[1]]
        )
      )
    }
  }

  if (is.data.frame(audit$referenceLevels) && nrow(audit$referenceLevels) > 0) {
    for (i in seq_len(nrow(audit$referenceLevels))) {
      row = audit$referenceLevels[i, , drop = FALSE]
      addRow(
        evidenceType = "referenceLevel",
        sourceSection = "referenceLevels",
        label = paste0("Reference level for `", row$predictor[[1]], "`"),
        summary = paste(
          "Reference level:",
          row$referenceLevel[[1]],
          "Available levels:",
          row$levels[[1]]
        )
      )
    }
  }

  if (is.list(audit$confidenceIntervals)) {
    addRow(
      evidenceType = "confidenceInterval",
      sourceSection = "confidenceIntervals",
      label = "Confidence interval rule",
      summary = paste(
        getExplanationTeachingConfidenceLevelPercent(audit = audit),
        "% confidence intervals.",
        getExplanationTeachingConfidenceNote(audit = audit)
      )
    )
  }

  if (is.data.frame(audit$baselineEvidence) && nrow(audit$baselineEvidence) > 0) {
    for (i in seq_len(nrow(audit$baselineEvidence))) {
      row = audit$baselineEvidence[i, , drop = FALSE]
      addRow(
        evidenceType = "baselineEvidence",
        sourceSection = "baselineEvidence",
        label = row$quantity[[1]] %||% paste0("Baseline quantity ", i),
        summary = paste(
          "Estimate:", as.character(row$estimate[[1]]),
          "Interval:", paste0("[", row$lower[[1]], ", ", row$upper[[1]], "]")
        )
      )
    }
  }

  if (is.data.frame(audit$effectEvidence) && nrow(audit$effectEvidence) > 0) {
    for (i in seq_len(nrow(audit$effectEvidence))) {
      row = audit$effectEvidence[i, , drop = FALSE]
      addRow(
        evidenceType = "effectEvidence",
        sourceSection = "effectEvidence",
        label = row$quantity[[1]] %||% paste0("Effect quantity ", i),
        summary = paste(
          "Estimate:", as.character(row$estimate[[1]]),
          "Interval:", paste0("[", row$lower[[1]], ", ", row$upper[[1]], "]")
        )
      )
    }
  }

  if (length(rows) == 0) {
    return(data.frame(
      evidenceId = character(0),
      evidenceType = character(0),
      sourceSection = character(0),
      label = character(0),
      summary = character(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, rows)
}

#' Check whether a teaching summary came from an explicit research-question input
#'
#' @param teachingSummary Optional teaching summary object.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
teachingSummaryHasExplicitResearchQuestion = function(teachingSummary = NULL) {

  isTRUE(attr(teachingSummary, "researchQuestionWasSupplied", exact = TRUE))
}

#' Remove an implicit final-answer tag when a prior research-question claim already exists
#'
#' @param claimTags Character vector of detected tags.
#' @param claimText Character scalar.
#' @param priorClaimTexts Character vector of earlier claim texts.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#'
#' @return Character vector.
#' @keywords internal
#' @noRd
maybeDropImplicitAnswerTag = function(
    claimTags,
    claimText,
    priorClaimTexts = character(0),
    teachingSummary = NULL,
    model = NULL
) {

  tags = as.character(claimTags %||% character(0))

  if (!("answer" %in% tags) || !("effect" %in% tags)) {
    return(tags)
  }

  if ("comparison" %in% tags || sentenceHasExplicitAnswerCue(claimText)) {
    return(tags)
  }

  if (!hasPriorResearchQuestionClaim(
    priorClaimTexts = priorClaimTexts,
    teachingSummary = teachingSummary,
    model = model
  )) {
    return(tags)
  }

  if (!teachingSummaryHasExplicitResearchQuestion(teachingSummary = teachingSummary)) {
    return(tags)
  }

  normaliseExplanationClaimTags(setdiff(tags, "answer"))
}

#' Check whether earlier claim texts already contain research-question framing
#'
#' @param priorClaimTexts Character vector.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
hasPriorResearchQuestionClaim = function(
    priorClaimTexts = character(0),
    teachingSummary = NULL,
    model = NULL
) {

  if (length(priorClaimTexts) == 0) {
    return(FALSE)
  }

  any(vapply(priorClaimTexts, function(text) {
    detectResearchQuestion(
      claimText = text,
      teachingSummary = teachingSummary,
      model = model
    )
  }, logical(1)))
}

#' Check whether a sentence has an explicit answer cue
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
sentenceHasExplicitAnswerCue = function(claimText) {

  text = trimws(tolower(claimText %||% ""))

  grepl(
    "^(overall\\b|in summary\\b|to answer the research question\\b|this suggests\\b|this shows\\b)",
    text,
    perl = TRUE
  )
}

#' Map one explanation sentence back to evidence
#'
#' @param claimId Character scalar.
#' @param sentenceIndex Integer sentence index.
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#' @param evidenceInventory Evidence inventory data frame.
#'
#' @return A single-row data frame.
#' @keywords internal
mapSingleExplanationClaim = function(
    claimId,
    sentenceIndex,
    claimText,
    audit,
    teachingSummary,
    model,
    evidenceInventory,
    totalClaims = NA_integer_,
    priorClaimTexts = character(0)
) {

  claimTags = detectExplanationClaimTags(
    claimText = claimText,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims
  )

  claimTags = maybeDropImplicitAnswerTag(
    claimTags = claimTags,
    claimText = claimText,
    priorClaimTexts = priorClaimTexts,
    teachingSummary = teachingSummary,
    model = model
  )

  claimType = deriveLegacyExplanationClaimType(claimTags = claimTags)

  matchedEvidence = selectExplanationEvidenceForClaim(
    claimTags = claimTags,
    claimType = claimType,
    claimText = claimText,
    audit = audit,
    teachingSummary = teachingSummary,
    evidenceInventory = evidenceInventory
  )

  roles = detectExplanationSentenceRoles(
    claimText = claimText,
    claimTags = claimTags,
    matchedEvidence = matchedEvidence
  )

  primaryRole = deriveExplanationPrimaryRole(roles)

  qualityFlags = buildExplanationSentenceQualityFlags(
    claimText = claimText,
    roles = roles
  )

  supportMapIds = buildExplanationSentenceSupportMapIds(
    matchedEvidence = matchedEvidence
  )

  supportNotes = buildExplanationClaimSupportNotes(
    claimTags = claimTags,
    matchedEvidence = matchedEvidence
  )

  supportNote = buildExplanationClaimSupportNote(
    claimTags = claimTags,
    matchedEvidence = matchedEvidence,
    supportNotes = supportNotes
  )

  data.frame(
    claimId = claimId,
    sentenceIndex = as.integer(sentenceIndex),
    claimText = claimText,
    claimTags = I(list(claimTags)),
    claimType = claimType,
    primaryRole = primaryRole,
    roles = I(list(roles)),
    qualityFlags = I(list(qualityFlags)),
    supportMapIds = I(list(supportMapIds)),
    supportNotes = I(list(supportNotes)),
    supportNote = supportNote,
    evidenceCount = nrow(matchedEvidence),
    evidenceTypes = paste(unique(stats::na.omit(matchedEvidence$evidenceType)), collapse = ", "),
    evidenceLabels = paste(unique(stats::na.omit(matchedEvidence$label)), collapse = " | "),
    mappingMethod = "keyword-audit hybrid",
    stringsAsFactors = FALSE
  )
}

#' Return the explicit legacy claim-type priority used for compatibility
#'
#' The multi-tag `claimTags` field is the primary representation. `claimType` is
#' a backward-compatibility label derived by ordered priority so older code can
#' continue to work predictably when a sentence carries multiple tags.
#'
#' @return A named character vector whose names are tags and whose values are the
#'   corresponding legacy `claimType` labels, ordered from highest to lowest
#'   priority.
#' @keywords internal
#' @noRd
getLegacyExplanationClaimTypePriority = function() {

  c(
    researchQuestion = "researchQuestion",
    answer = "answer",
    typicalCase = "baseline",
    effect = "mainEffect",
    comparison = "comparison",
    uncertainty = "uncertainty",
    scale = "scale"
  )
}

#' Derive the legacy single-type label from claim tags
#'
#' The compatibility rule is explicit: `claimType` is assigned by the first
#' matching entry in `getLegacyExplanationClaimTypePriority()`. This means that
#' sentences with `answer` plus other tags still derive `claimType = "answer"`,
#' while `researchQuestion` always remains the top-priority legacy label.
#'
#' @param claimTags Character vector of detected claim tags.
#'
#' @return A single character string.
#' @keywords internal
#' @noRd
deriveLegacyExplanationClaimType = function(claimTags) {

  tags = normaliseExplanationClaimTags(claimTags)

  if (length(tags) == 0) {
    return("general")
  }

  priority = getLegacyExplanationClaimTypePriority()

  for (tag in names(priority)) {
    if (tag %in% tags) {
      return(unname(priority[[tag]]))
    }
  }

  "general"
}

#' Classify a sentence-level explanation claim for legacy compatibility
#'
#' This helper now derives the legacy single-label `claimType` value from the
#' multi-tag detector output used by the claim-mapping pipeline. New code
#' should prefer `detectExplanationClaimTags()` and treat `claimType` as a
#' compatibility field only.
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#' @param sentenceIndex Integer-like sentence index.
#' @param totalClaims Integer-like total number of claims.
#' @param claimTags Optional character vector of detected claim tags.
#'
#' @return A single character string.
#' @keywords internal
classifyExplanationClaimType = function(
    claimText,
    audit,
    teachingSummary = NULL,
    model = NULL,
    sentenceIndex = NA_integer_,
    totalClaims = NA_integer_,
    claimTags = NULL
) {

  if (is.null(claimTags)) {
    claimTags = detectExplanationClaimTags(
      claimText = claimText,
      audit = audit,
      teachingSummary = teachingSummary,
      model = model,
      sentenceIndex = sentenceIndex,
      totalClaims = totalClaims
    )
  }

  deriveLegacyExplanationClaimType(claimTags = claimTags)
}

#' Select evidence rows for a sentence-level claim
#'
#' @param claimTags Optional character vector of detected tags.
#' @param claimType Optional legacy single-type label kept for compatibility.
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param evidenceInventory Evidence inventory data frame.
#'
#' @return A data frame.
#' @keywords internal
selectExplanationEvidenceForClaim = function(
    claimTags = NULL,
    claimType = NULL,
    claimText,
    audit,
    teachingSummary,
    evidenceInventory
) {

  if (!is.data.frame(evidenceInventory) || nrow(evidenceInventory) == 0) {
    return(data.frame(
      evidenceId = character(0),
      evidenceType = character(0),
      sourceSection = character(0),
      label = character(0),
      summary = character(0),
      stringsAsFactors = FALSE
    ))
  }

  tags = normaliseExplanationClaimTags(claimTags)

  if (length(tags) == 0 && is.character(claimType) && length(claimType) == 1 && !is.na(claimType)) {
    tags = switch(
      claimType,
      researchQuestion = "researchQuestion",
      answer = "answer",
      uncertainty = "uncertainty",
      scale = "scale",
      baseline = "typicalCase",
      comparison = "comparison",
      mainEffect = "effect",
      character(0)
    )
  }

  keepTypes = unique(unlist(lapply(tags, function(tag) {
    switch(
      tag,
      researchQuestion = c("researchQuestion"),
      typicalCase = c("baselineChoice", "numericAnchor", "baselineEvidence"),
      effect = c("mainEffect", "effectEvidence", "scale"),
      uncertainty = c("uncertainty", "confidenceInterval", "effectEvidence", "baselineEvidence"),
      comparison = c("referenceLevel", "effectEvidence", "mainEffect"),
      answer = c("researchQuestion", "mainEffect", "referenceLevel", "uncertainty", "confidenceInterval", "effectEvidence", "scale"),
      scale = c("scale"),
      character(0)
    )
  })))

  if (length(keepTypes) == 0) {
    keepTypes = selectContextEvidenceTypes(
      claimText = claimText,
      evidenceInventory = evidenceInventory
    )
  }

  out = evidenceInventory[evidenceInventory$evidenceType %in% keepTypes, , drop = FALSE]

  if ("typicalCase" %in% tags && is.list(audit$numericAnchor) && is.data.frame(audit$numericAnchor$table) && nrow(audit$numericAnchor$table) > 0) {
    text = tolower(claimText %||% "")
    matchedPredictors = vapply(seq_len(nrow(audit$numericAnchor$table)), function(i) {
      row = audit$numericAnchor$table[i, , drop = FALSE]
      grepl(tolower(row$predictor[[1]]), text, fixed = TRUE) ||
        grepl(normaliseExplanationNumericString(row$anchor[[1]]), text, fixed = TRUE)
    }, logical(1))

    predictorNames = audit$numericAnchor$table$predictor[matchedPredictors]

    if (length(predictorNames) > 0) {
      numericAnchorKeep = vapply(seq_len(nrow(out)), function(i) {
        if (!identical(out$evidenceType[[i]], "numericAnchor")) {
          return(TRUE)
        }

        any(vapply(predictorNames, function(name) {
          grepl(paste0("`", name, "`"), out$label[[i]], fixed = TRUE)
        }, logical(1)))
      }, logical(1))

      out = out[numericAnchorKeep, , drop = FALSE]
    }
  }

  unique(out)
}


#' Apply structural answer selection across mapped explanation claims
#'
#' @param claimsTable Claim mapping data frame.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#' @param evidenceInventory Evidence inventory data frame.
#'
#' @return A claim mapping data frame.
#' @keywords internal
#' @noRd
applyStructuralAnswerSelection = function(
    claimsTable,
    audit,
    teachingSummary = NULL,
    model = NULL,
    evidenceInventory
) {

  if (!is.data.frame(claimsTable) || nrow(claimsTable) == 0) {
    return(claimsTable)
  }

  adjustedTags = lapply(seq_len(nrow(claimsTable)), function(i) {
    tags = normaliseExplanationClaimTags(claimsTable$claimTags[[i]])
    setdiff(tags, "answer")
  })

  candidateIndex = selectStructuralAnswerClaimIndex(
    claimTexts = claimsTable$claimText,
    claimTags = adjustedTags,
    teachingSummary = teachingSummary,
    model = model
  )

  if (!is.na(candidateIndex)) {
    adjustedTags[[candidateIndex]] = normaliseExplanationClaimTags(c(adjustedTags[[candidateIndex]], "answer"))
  }

  rebuiltClaims = lapply(seq_len(nrow(claimsTable)), function(i) {
    claimTags = adjustedTags[[i]]
    claimType = deriveLegacyExplanationClaimType(claimTags = claimTags)
    matchedEvidence = selectExplanationEvidenceForClaim(
      claimTags = claimTags,
      claimType = claimType,
      claimText = claimsTable$claimText[[i]],
      audit = audit,
      teachingSummary = teachingSummary,
      evidenceInventory = evidenceInventory
    )
    roles = mapExplanationClaimTagsToSentenceRoles(
      claimTags = claimTags,
      matchedEvidence = matchedEvidence,
      claimText = claimsTable$claimText[[i]]
    )
    primaryRole = deriveExplanationPrimaryRole(roles)
    qualityFlags = buildExplanationSentenceQualityFlags(
      claimText = claimsTable$claimText[[i]],
      roles = roles
    )
    supportMapIds = buildExplanationSentenceSupportMapIds(
      matchedEvidence = matchedEvidence
    )
    supportNotes = buildExplanationClaimSupportNotes(
      claimTags = claimTags,
      matchedEvidence = matchedEvidence
    )
    supportNote = buildExplanationClaimSupportNote(
      claimTags = claimTags,
      matchedEvidence = matchedEvidence,
      supportNotes = supportNotes
    )
    data.frame(
      claimId = claimsTable$claimId[[i]],
      sentenceIndex = claimsTable$sentenceIndex[[i]],
      claimText = claimsTable$claimText[[i]],
      claimTags = I(list(claimTags)),
      claimType = claimType,
      primaryRole = primaryRole,
      roles = I(list(roles)),
      qualityFlags = I(list(qualityFlags)),
      supportMapIds = I(list(supportMapIds)),
      supportNotes = I(list(supportNotes)),
      supportNote = supportNote,
      evidenceCount = nrow(matchedEvidence),
      evidenceTypes = paste(unique(stats::na.omit(matchedEvidence$evidenceType)), collapse = ", "),
      evidenceLabels = paste(unique(stats::na.omit(matchedEvidence$label)), collapse = " | "),
      mappingMethod = claimsTable$mappingMethod[[i]],
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rebuiltClaims)
}

#' Select the last substantive answer candidate from mapped claims
#'
#' @param claimTexts Character vector of claim texts.
#' @param claimTags List of tag vectors with any implicit `answer` removed.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#'
#' @return Integer index or `NA_integer_`.
#' @keywords internal
#' @noRd
selectStructuralAnswerClaimIndex = function(
    claimTexts,
    claimTags,
    teachingSummary = NULL,
    model = NULL
) {

  researchQuestion = trimws(getStoredResearchQuestion(
    teachingSummary = teachingSummary,
    model = model
  ))

  if (!nzchar(researchQuestion) || length(claimTexts) == 0) {
    return(NA_integer_)
  }

  for (i in rev(seq_along(claimTexts))) {
    if (isStructuralAnswerCandidate(
      claimText = claimTexts[[i]],
      claimTags = claimTags[[i]],
      teachingSummary = teachingSummary,
      model = model,
      priorClaimTexts = if (i > 1L) claimTexts[seq_len(i - 1L)] else character(0)
    )) {
      return(as.integer(i))
    }
  }

  NA_integer_
}

#' Check whether a mapped sentence is a substantive answer candidate
#'
#' @param claimText Character scalar.
#' @param claimTags Character vector.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#' @param priorClaimTexts Character vector of earlier claim texts.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
isStructuralAnswerCandidate = function(
    claimText,
    claimTags,
    teachingSummary = NULL,
    model = NULL,
    priorClaimTexts = character(0)
) {

  tags = normaliseExplanationClaimTags(claimTags)

  if ("researchQuestion" %in% tags) {
    return(FALSE)
  }

  text = trimws(tolower(claimText %||% ""))

  if (!nzchar(text) || sentenceIsDisclaimerTail(claimText)) {
    return(FALSE)
  }

  if (isInterceptOnlyExplanationModel(model) &&
      "typicalCase" %in% tags &&
      "uncertainty" %in% tags &&
      sentenceLooksLikePointEstimate(claimText)) {
    return(TRUE)
  }

  if (sentenceHasExplicitAnswerCue(claimText)) {
    return(TRUE)
  }

  if ("typicalCase" %in% tags || !("effect" %in% tags)) {
    return(FALSE)
  }

  if ("comparison" %in% tags) {
    return(TRUE)
  }

  if ("uncertainty" %in% tags) {
    return(FALSE)
  }

  if (
    hasPriorResearchQuestionClaim(
      priorClaimTexts = priorClaimTexts,
      teachingSummary = teachingSummary,
      model = model
    ) &&
    teachingSummaryHasExplicitResearchQuestion(teachingSummary = teachingSummary)
  ) {
    return(FALSE)
  }

  TRUE
}

#' Check whether a sentence is a trailing disclaimer rather than an answer
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
sentenceIsDisclaimerTail = function(claimText) {

  text = trimws(tolower(claimText %||% ""))

  grepl(
    "does not guarantee individual|does not predict individual|does not guarantee that every individual|applies on average|reflects average effects|average effects and does not|individual outcomes",
    text,
    perl = TRUE
  )
}

#' Select context evidence types for untagged setup sentences
#'
#' @param claimText Character scalar.
#' @param evidenceInventory Evidence inventory data frame.
#'
#' @return Character vector of evidence types.
#' @keywords internal
#' @noRd
selectContextEvidenceTypes = function(claimText, evidenceInventory) {

  if (!is.data.frame(evidenceInventory) || nrow(evidenceInventory) == 0) {
    return(character(0))
  }

  claimTokens = tokenizeExplanationClaimText(claimText)

  overlaps = vapply(seq_len(nrow(evidenceInventory)), function(i) {
    evidenceTokens = tokenizeExplanationClaimText(paste(
      evidenceInventory$label[[i]],
      evidenceInventory$summary[[i]]
    ))
    length(intersect(claimTokens, evidenceTokens))
  }, numeric(1))

  bestOverlap = max(overlaps, 0)

  if (bestOverlap > 0) {
    return(unique(evidenceInventory$evidenceType[overlaps == bestOverlap]))
  }

  if ("dataDescription" %in% evidenceInventory$evidenceType) {
    return("dataDescription")
  }

  character(0)
}

#' Tokenize a claim or question for overlap matching
#'
#' @param text Character scalar.
#'
#' @return Character vector.
#' @keywords internal
tokenizeExplanationClaimText = function(text) {

  x = tolower(text %||% "")
  x = gsub("[^a-z0-9 ]", " ", x)
  tokens = unlist(strsplit(x, "[[:space:]]+"), use.names = FALSE)
  tokens = tokens[nzchar(tokens)]
  setdiff(tokens, c("the", "and", "for", "with", "that", "this", "from", "into", "than", "does"))
}

#' Normalise a numeric value for simple text matching
#'
#' @param x Numeric-like scalar.
#'
#' @return Character scalar.
#' @keywords internal
normaliseExplanationNumericString = function(x) {

  value = suppressWarnings(as.numeric(x))[1]

  if (is.na(value)) {
    return(trimws(as.character(x)[1] %||% ""))
  }

  out = format(round(value, 2), trim = TRUE, scientific = FALSE)
  trimws(out)
}
