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
#' The current implementation uses a hybrid approach:
#' - deterministic evidence inventory from the audit and teaching summary
#' - lightweight sentence-level matching against the final explanation text
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
      "Sentence-level hybrid matching using the deterministic audit, the teaching summary,",
      "and lightweight text heuristics."
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

  claimType = classifyExplanationClaimType(
    claimText = claimText,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims,
    claimTags = claimTags
  )

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

#' Classify a sentence-level explanation claim
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
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

#' Detect one or more tags for a sentence-level explanation claim
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#' @param sentenceIndex Integer-like sentence index.
#' @param totalClaims Integer-like total number of claims.
#'
#' @return Character vector.
#' @keywords internal
#' @examples
#' Detect one or more tags for a sentence-level explanation claim
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#' @param sentenceIndex Optional integer sentence index.
#' @param totalClaims Optional integer total number of claims.
#'
#' @return A character vector of zero or more claim tags.
#' @keywords internal
#' @noRd
detectExplanationClaimTags = function(
    claimText,
    audit,
    teachingSummary = NULL,
    model = NULL,
    sentenceIndex = NA_integer_,
    totalClaims = NA_integer_
) {

  text = trimws(claimText %||% "")

  if (!nzchar(text)) {
    return(character(0))
  }

  if (detectResearchQuestion(claimText = claimText, teachingSummary = teachingSummary, model = model)) {
    return("researchQuestion")
  }

  if (detectAnswer(
    claimText = claimText,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims,
    model = model,
    teachingSummary = teachingSummary
  )) {
    return("answer")
  }

  tags = c(
    if (detectTypicalCase(claimText = claimText, audit = audit)) "typicalCase",
    if (detectEffect(claimText = claimText, audit = audit, model = model)) "effect",
    if (detectUncertainty(claimText = claimText)) "uncertainty",
    if (detectComparison(claimText = claimText, audit = audit)) "comparison",
    if (detectScale(claimText = claimText)) "scale"
  )

  normaliseExplanationClaimTags(tags)
}

#' Detect research-question framing in a sentence
#'
#' @param claimText Character scalar.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#'
#' @return Logical scalar.
#' @keywords internal
detectResearchQuestion = function(claimText, teachingSummary = NULL, model = NULL) {

  directResearchQuestion = trimws(as.character(attr(model, "wmfm_research_question", exact = TRUE) %||% ""))

  if (nzchar(directResearchQuestion) && sentenceMatchesResearchQuestion(claimText, directResearchQuestion)) {
    return(TRUE)
  }

  summaryResearchQuestion = trimws(as.character(teachingSummary$researchQuestionLink %||% ""))

  nzchar(summaryResearchQuestion) && sentenceMatchesResearchQuestion(claimText, summaryResearchQuestion)
}

#' Detect typical-case framing in a sentence
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return Logical scalar.
#' @keywords internal
detectTypicalCase = function(claimText, audit) {
  sentenceMatchesBaselineEvidence(claimText = claimText, audit = audit)
}

#' Detect effect wording in a sentence
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param model Optional fitted model object.
#'
#' @return Logical scalar.
#' @keywords internal
detectEffect = function(claimText, audit, model = NULL) {
  sentenceMentionsModelledChange(claimText = claimText, audit = audit, model = model)
}

#' Detect uncertainty wording in a sentence
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
detectUncertainty = function(claimText) {

  text = tolower(claimText %||% "")

  grepl(
    "confidence interval|confidence intervals|confidence limit|confidence limits|uncertain|uncertainty|likely|plausible|consistent with|confidence band|confidence bands|interval estimate",
    text,
    perl = TRUE
  )
}

#' Detect comparison wording in a sentence
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return Logical scalar.
#' @keywords internal
detectComparison = function(claimText, audit) {
  sentenceMatchesReferenceLevel(claimText = claimText, audit = audit)
}

#' Detect final-answer wording in a sentence
#'
#' @param claimText Character scalar.
#' @param sentenceIndex Integer-like sentence index.
#' @param totalClaims Integer-like total number of claims.
#' @param model Optional fitted model object.
#' @param teachingSummary Optional teaching summary object.
#'
#' @return Logical scalar.
#' @keywords internal
detectAnswer = function(
    claimText,
    sentenceIndex = NA_integer_,
    totalClaims = NA_integer_,
    model = NULL,
    teachingSummary = NULL
) {

  sentenceLooksLikeResearchAnswer(
    claimText = claimText,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims,
    model = model,
    teachingSummary = teachingSummary
  )
}

#' Detect response-scale wording in a sentence
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
detectScale = function(claimText) {

  text = tolower(claimText %||% "")
  grepl(
    "odds scale|probability scale|response scale|log-odds|on the odds scale|on the probability scale|on the response scale|mean response scale|expected count scale|expected value scale",
    text,
    perl = TRUE
  )
}

#' Normalize claim-tag order for stable display and testing
#'
#' @param claimTags Character vector.
#'
#' @return Character vector.
#' @keywords internal
normaliseExplanationClaimTags = function(claimTags) {

  allowedOrder = c(
    "researchQuestion",
    "typicalCase",
    "effect",
    "uncertainty",
    "comparison",
    "answer",
    "scale"
  )

  tags = unique(stats::na.omit(as.character(claimTags %||% character(0))))
  tags = tags[tags %in% allowedOrder]
  allowedOrder[allowedOrder %in% tags]
}

#' Check whether a sentence matches research-question framing
#'
#' @param claimText Character scalar.
#' @param researchQuestion Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
sentenceMatchesResearchQuestion = function(claimText, researchQuestion) {

  sentence = trimws(tolower(claimText %||% ""))
  question = trimws(tolower(researchQuestion %||% ""))

  if (!nzchar(sentence) || !nzchar(question)) {
    return(FALSE)
  }

  if (identical(sentence, question)) {
    return(TRUE)
  }

  sentenceStem = trimws(sub("[?!.]+$", "", sentence))
  questionStem = trimws(sub("[?!.]+$", "", question))

  if (identical(sentenceStem, questionStem) && grepl("[?]$", trimws(claimText %||% ""))) {
    return(TRUE)
  }

  if (grepl("^(the )?(study|question|research question) asks\\b", sentenceStem, perl = TRUE)) {
    return(TRUE)
  }

  sentenceTokens = tokenizeExplanationClaimText(claimText)
  questionTokens = tokenizeExplanationClaimText(researchQuestion)

  overlap = intersect(sentenceTokens, questionTokens)
  tokenDenominator = max(length(unique(questionTokens)), 1)
  overlapShare = length(overlap) / tokenDenominator

  if (grepl("\\b(how|whether)\\b", sentenceStem, perl = TRUE) && overlapShare >= 0.2 && length(overlap) >= 2) {
    return(TRUE)
  }

  if (grepl("[?]$", trimws(claimText %||% "")) && length(overlap) >= 2 && overlapShare >= 0.4) {
    return(TRUE)
  }

  leadingQuestionCue = grepl("^(does|do|did|can|could|should|would|is|are|will|has|have)\\b", sentenceStem, perl = TRUE)

  if (leadingQuestionCue && grepl("[?]$", trimws(claimText %||% "")) && overlapShare >= 0.3) {
    return(TRUE)
  }

  FALSE
}

#' Check whether a sentence matches baseline evidence
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return Logical scalar.
#' @keywords internal
sentenceMatchesBaselineEvidence = function(claimText, audit) {

  text = tolower(claimText %||% "")

  baselineCue = grepl(
    "\\bwhen\\b|at about|at around|at the average|at an average|at a value|at a magnitude of|at the typical|typical magnitude|holding|for a student|for someone|starting value|baseline|fitted value",
    text,
    perl = TRUE
  )

  anchorTerms = character(0)

  if (is.list(audit$numericAnchor) && is.data.frame(audit$numericAnchor$table) && nrow(audit$numericAnchor$table) > 0) {
    anchorTerms = unlist(lapply(seq_len(nrow(audit$numericAnchor$table)), function(i) {
      row = audit$numericAnchor$table[i, , drop = FALSE]
      c(
        normaliseExplanationNumericString(row$anchor[[1]])
      )
    }), use.names = FALSE)
  }

  baselineEvidenceLabels = character(0)
  if (is.data.frame(audit$baselineEvidence) && nrow(audit$baselineEvidence) > 0 && "quantity" %in% names(audit$baselineEvidence)) {
    baselineEvidenceLabels = tolower(stats::na.omit(as.character(audit$baselineEvidence$quantity)))
  }

  termMatch = any(vapply(anchorTerms, function(term) {
    nzchar(term) && grepl(term, text, fixed = TRUE)
  }, logical(1)))

  labelMatch = any(vapply(baselineEvidenceLabels, function(label) {
    nzchar(label) && grepl(label, text, fixed = TRUE)
  }, logical(1)))

  anchoredExpectedCue = termMatch &&
    grepl(
      "expected count|expected value|are expected to occur|is expected to occur|are expected|is expected|predicts an average|average of about|average of",
      text,
      perl = TRUE
    )

  baselineCue || labelMatch || anchoredExpectedCue || (baselineCue && termMatch)
}

#' Check whether a sentence matches factor reference-level evidence
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return Logical scalar.
#' @keywords internal
sentenceMatchesReferenceLevel = function(claimText, audit) {

  text = tolower(claimText %||% "")

  if (!is.data.frame(audit$referenceLevels) || nrow(audit$referenceLevels) == 0) {
    return(FALSE)
  }

  cueMatch = grepl(
    "compares|compared with|compared to|relative to|reference group|reference level|other group|comparison",
    text,
    perl = TRUE
  )

  if (!cueMatch) {
    return(FALSE)
  }

  any(vapply(seq_len(nrow(audit$referenceLevels)), function(i) {
    row = audit$referenceLevels[i, , drop = FALSE]
    predictorMatch = grepl(tolower(row$predictor[[1]]), text, fixed = TRUE)
    referenceMatch = grepl(tolower(row$referenceLevel[[1]]), text, fixed = TRUE)
    predictorMatch || referenceMatch
  }, logical(1))) || grepl("reference group|reference level|other group", text, perl = TRUE)
}

#' Check whether a sentence mentions a modelled change
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param model Optional fitted model object.
#'
#' @return Logical scalar.
#' @keywords internal
sentenceMentionsModelledChange = function(claimText, audit, model = NULL) {

  text = tolower(claimText %||% "")
  changeCue = grepl(
    "increase|decrease|higher|lower|changes|associated with|tend to|falls|rises|decline|declines|drop|drops|steeper",
    text,
    perl = TRUE
  )
  multiplierCue = grepl(
    "multiplies|multiplied|1-unit|one-unit|one magnitude|one-magnitude|per unit|per one-unit",
    text,
    perl = TRUE
  )

  predictorNames = character(0)

  if (!is.null(model)) {
    mf = tryCatch(stats::model.frame(model), error = function(e) NULL)

    if (is.data.frame(mf) && ncol(mf) >= 2) {
      predictorNames = tolower(names(mf)[-1])
    }
  }

  predictorMatch = length(predictorNames) > 0 && any(vapply(predictorNames, function(name) {
    grepl(name, text, fixed = TRUE)
  }, logical(1)))

  effectLabelMatch = FALSE

  if (is.data.frame(audit$effectEvidence) && nrow(audit$effectEvidence) > 0 && "quantity" %in% names(audit$effectEvidence)) {
    effectLabels = tolower(stats::na.omit(as.character(audit$effectEvidence$quantity)))
    effectLabelMatch = any(vapply(effectLabels, function(label) {
      grepl(label, text, fixed = TRUE)
    }, logical(1)))
  }

  uncertaintyQualifiedChangeCue = changeCue && grepl(
    "confidence interval|confidence intervals|confidence limit|confidence limits|likely|plausible|consistent with|positive|negative",
    text,
    perl = TRUE
  )

  effectLabelMatch || multiplierCue || (changeCue && predictorMatch) || (changeCue && grepl("\\bas\\b", text, perl = TRUE)) || uncertaintyQualifiedChangeCue
}

#' Select evidence rows for a sentence-level claim
#'
#' @param claimTags Optional character vector of detected tags.
#' @param claimType Optional legacy single-type label.
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


#' Check whether a sentence looks like the final answer to the research question
#'
#' @param claimText Character scalar.
#' @param sentenceIndex Integer-like sentence index.
#' @param totalClaims Integer-like total number of claims.
#' @param model Optional fitted model object.
#' @param teachingSummary Optional teaching summary object.
#'
#' @return Logical scalar.
#' @keywords internal
sentenceLooksLikeResearchAnswer = function(
    claimText,
    sentenceIndex = NA_integer_,
    totalClaims = NA_integer_,
    model = NULL,
    teachingSummary = NULL
) {

  text = trimws(tolower(claimText %||% ""))

  if (!nzchar(text)) {
    return(FALSE)
  }

  isNearEnd = !is.na(sentenceIndex) && !is.na(totalClaims) &&
    sentenceIndex >= max(1L, totalClaims - 1L)

  answerCue = grepl(
    "^answer:|^on average\b|^overall\b|^in summary\b|^in short\b|^thus\b|^therefore\b|the data are consistent with|the data indicate|the data suggest|suggest that|indicate that|declines more steeply|falls more steeply|rises more steeply|rate of decline|stronger than|steeper than",
    text,
    perl = TRUE
  )

  if (!(isNearEnd && answerCue)) {
    return(FALSE)
  }

  researchQuestion = trimws(as.character(
    attr(model, "wmfm_research_question", exact = TRUE) %||%
      teachingSummary$researchQuestionLink %||%
      ""
  ))

  if (!nzchar(researchQuestion)) {
    return(FALSE)
  }

  overlap = intersect(
    tokenizeExplanationClaimText(claimText),
    tokenizeExplanationClaimText(researchQuestion)
  )

  length(overlap) >= 1 ||
    grepl("the data are consistent with|the data indicate|the data suggest|suggest that|indicate that", text, perl = TRUE)
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
