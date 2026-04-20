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
      claimType = character(0),
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

  claimType = classifyExplanationClaimType(
    claimText = claimText,
    audit = audit,
    teachingSummary = teachingSummary,
    model = model,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims
  )

  matchedEvidence = selectExplanationEvidenceForClaim(
    claimType = claimType,
    claimText = claimText,
    audit = audit,
    teachingSummary = teachingSummary,
    evidenceInventory = evidenceInventory
  )

  supportNote = buildExplanationClaimSupportNote(
    claimType = claimType,
    matchedEvidence = matchedEvidence
  )

  data.frame(
    claimId = claimId,
    sentenceIndex = as.integer(sentenceIndex),
    claimText = claimText,
    claimType = claimType,
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
#' Check whether a sentence contains explicit uncertainty language
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
sentenceHasUncertaintyCue = function(claimText) {

  text = tolower(claimText %||% "")

  grepl(
    "confidence interval|confidence intervals|confidence limits|limits|plausible|likely|uncertain|uncertainty|consistent with|bounds|range",
    text,
    perl = TRUE
  )
}

#' Check whether a sentence contains a strong change statement
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
sentenceHasStrongChangeCue = function(claimText) {

  text = tolower(claimText %||% "")

  grepl(
    "for each|each one-point|1-unit increase|one-point increase|per unit|per magnitude unit|multiplies|drops to|cuts the expected count|falls by|rises by|same one-point rise",
    text,
    perl = TRUE
  )
}

#' Check whether a sentence is primarily about uncertainty
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
sentenceIsPrimarilyUncertainty = function(claimText) {

  text = tolower(claimText %||% "")

  if (!sentenceHasUncertaintyCue(claimText)) {
    return(FALSE)
  }

  leadingUncertaintyCue = grepl(
    "^(these|the) confidence interval|^(these|the) confidence intervals|^(these|the) confidence limits|^(the )?interval|^(the )?limits",
    text,
    perl = TRUE
  )

  interpretiveUncertaintyCue = grepl(
    "suggest|indicating|consistent with|plausible|likely",
    text,
    perl = TRUE
  )

  (leadingUncertaintyCue || interpretiveUncertaintyCue) && !sentenceHasStrongChangeCue(claimText)
}

classifyExplanationClaimType = function(
    claimText,
    audit,
    teachingSummary = NULL,
    model = NULL,
    sentenceIndex = NA_integer_,
    totalClaims = NA_integer_
) {

  text = tolower(claimText %||% "")

  if (!nzchar(text)) {
    return("general")
  }

  directResearchQuestion = trimws(as.character(attr(model, "wmfm_research_question", exact = TRUE) %||% ""))

  if (nzchar(directResearchQuestion) && sentenceMatchesResearchQuestion(claimText, directResearchQuestion)) {
    return("researchQuestion")
  }

  if (grepl("confidence interval|confidence intervals|uncertain|uncertainty|likely|plausible|consistent with", text, perl = TRUE)) {
    return("uncertainty")
  }

  if (sentenceLooksLikeResearchAnswer(
    claimText = claimText,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims,
    model = model,
    teachingSummary = teachingSummary
  )) {
    return("answer")
  }

  summaryResearchQuestion = trimws(as.character(teachingSummary$researchQuestionLink %||% ""))

  if (nzchar(summaryResearchQuestion) && sentenceMatchesResearchQuestion(claimText, summaryResearchQuestion)) {
    return("researchQuestion")
  }

  if (sentenceMatchesBaselineEvidence(claimText, audit)) {
    return("baseline")
  }

  if (sentenceMentionsModelledChange(claimText, audit, model)) {
    return("mainEffect")
  }

  if (sentenceMatchesReferenceLevel(claimText, audit)) {
    return("comparison")
  }

  if (grepl("odds|probability|expected count|expected value|mean response|response scale", text, perl = TRUE)) {
    return("scale")
  }

  "general"
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
    "\\bwhen\\b|at about|at around|at the average|at an average|at a value|holding|for a student|for someone|starting value|baseline|fitted value",
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

  baselineCue || labelMatch || (baselineCue && termMatch)
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

  cueMatch = grepl("compared with|compared to|relative to|reference group|reference level|other group|group", text, perl = TRUE)

  levelMatch = any(vapply(seq_len(nrow(audit$referenceLevels)), function(i) {
    row = audit$referenceLevels[i, , drop = FALSE]
    predictorMatch = grepl(tolower(row$predictor[[1]]), text, fixed = TRUE)
    referenceMatch = grepl(tolower(row$referenceLevel[[1]]), text, fixed = TRUE)
    predictorMatch || referenceMatch
  }, logical(1)))

  cueMatch || levelMatch
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
  changeCue = grepl("increase|decrease|higher|lower|changes|associated with|tend to", text, perl = TRUE)

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

  changeCue || predictorMatch || effectLabelMatch
}

#' Select evidence rows for a sentence-level claim
#'
#' @param claimType Character scalar.
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param evidenceInventory Evidence inventory data frame.
#'
#' @return A data frame.
#' @keywords internal
selectExplanationEvidenceForClaim = function(
    claimType,
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

  keepTypes = switch(
    claimType,
    researchQuestion = c("researchQuestion"),
    answer = c("researchQuestion", "mainEffect", "comparison", "uncertainty", "effectEvidence", "scale"),
    uncertainty = c("uncertainty", "confidenceInterval", "effectEvidence", "baselineEvidence"),
    scale = c("scale"),
    baseline = c("baselineChoice", "numericAnchor", "baselineEvidence"),
    comparison = c("referenceLevel", "effectEvidence", "mainEffect"),
    mainEffect = c("mainEffect", "effectEvidence", "scale"),
    c("mainEffect", "effectEvidence", "confidenceInterval")
  )

  out = evidenceInventory[evidenceInventory$evidenceType %in% keepTypes, , drop = FALSE]

  if (identical(claimType, "baseline") && is.list(audit$numericAnchor) && is.data.frame(audit$numericAnchor$table) && nrow(audit$numericAnchor$table) > 0) {
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

  isLastSentence = !is.na(sentenceIndex) && !is.na(totalClaims) && sentenceIndex == totalClaims
  answerCue = grepl(
    "^answer:|^on average\b|^overall\b|^in summary\b|the data are consistent with|tend to|associated with",
    text,
    perl = TRUE
  )

  if (!(isLastSentence && answerCue)) {
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

  length(overlap) >= 1 || grepl("the data are consistent with", text, fixed = TRUE)
}

#' Build a plain-language support note for a mapped claim
#'
#' @param claimType Character scalar.
#' @param matchedEvidence Data frame of evidence rows.
#'
#' @return A single character string.
#' @keywords internal
buildExplanationClaimSupportNote = function(claimType, matchedEvidence) {

  switch(
    claimType,
    researchQuestion = "This sentence restates the research question in plain language.",
    answer = "This sentence gives the overall answer to the research question.",
    uncertainty = "This sentence explains the uncertainty around the estimate and uses confidence-interval guidance to keep the wording cautious.",
    scale = "This sentence explains the scale used to describe the response.",
    baseline = "This sentence describes a typical case and its expected outcome.",
    comparison = "This sentence explains how the groups are being compared.",
    mainEffect = "This sentence explains how the response changes as the predictor increases.",
    "This sentence provides supporting context for the explanation."
  )
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
