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
      totalClaims = length(claimTexts)
    )
  })

  claimsTable = if (length(claims) == 0) {
    data.frame(
      claimId = character(0),
      sentenceIndex = integer(0),
      claimText = character(0),
      claimTags = I(list()),
      claimType = character(0),
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
        round((audit$confidenceIntervals$level %||% 0.95) * 100),
        "% confidence intervals.",
        audit$confidenceIntervals$teachingNote %||% audit$confidenceIntervals$note %||% ""
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
    totalClaims = NA_integer_
) {

  claimTags = detectExplanationClaimTags(
    claimText = claimText,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims
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
    supportNotes = I(list(supportNotes)),
    supportNote = supportNote,
    evidenceCount = nrow(matchedEvidence),
    evidenceTypes = paste(unique(stats::na.omit(matchedEvidence$evidenceType)), collapse = ", "),
    evidenceLabels = paste(unique(stats::na.omit(matchedEvidence$label)), collapse = " | "),
    mappingMethod = "keyword-audit hybrid",
    stringsAsFactors = FALSE
  )
}

#' Derive the legacy single-type label from claim tags
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

  if ("researchQuestion" %in% tags) {
    return("researchQuestion")
  }

  if ("answer" %in% tags) {
    return("answer")
  }

  if ("typicalCase" %in% tags) {
    return("baseline")
  }

  if ("effect" %in% tags) {
    return("mainEffect")
  }

  if ("comparison" %in% tags) {
    return("comparison")
  }

  if ("uncertainty" %in% tags) {
    return("uncertainty")
  }

  if ("scale" %in% tags) {
    return("scale")
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
    keepTypes = c("mainEffect", "effectEvidence", "confidenceInterval")
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
    switch(
      tag,
      researchQuestion = "restates the research question",
      typicalCase = "describes a typical case",
      effect = "explains how the response changes",
      uncertainty = "shows uncertainty in the estimate",
      comparison = "describes how groups are being compared",
      answer = "helps answer the research question",
      scale = "explains the response scale",
      "provides supporting context"
    )
  }, character(1))

  unique(notes)
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
