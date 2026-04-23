#' Detect one or more tags for a sentence-level explanation claim
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#' @param sentenceIndex Integer-like sentence index.
#' @param totalClaims Integer-like total number of claims.
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

  if (detectResearchQuestion(
    claimText = claimText,
    teachingSummary = teachingSummary,
    model = model
  )) {
    return("researchQuestion")
  }

  typicalCaseTag = detectTypicalCase(claimText = claimText, audit = audit)
  effectTag = detectEffect(claimText = claimText, audit = audit, model = model)
  uncertaintyTag = detectUncertainty(claimText = claimText)
  comparisonTag = detectComparison(claimText = claimText, audit = audit)
  answerTag = detectAnswer(
    claimText = claimText,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims,
    model = model,
    teachingSummary = teachingSummary,
    effectTag = effectTag,
    uncertaintyTag = uncertaintyTag,
    comparisonTag = comparisonTag,
    typicalCaseTag = typicalCaseTag
  )

  tags = c(
    if (typicalCaseTag) "typicalCase",
    if (effectTag) "effect",
    if (uncertaintyTag) "uncertainty",
    if (comparisonTag) "comparison",
    if (answerTag) "answer",
    if (detectScale(claimText = claimText)) "scale"
  )

  normaliseExplanationClaimTags(tags)
}


#' Recover the explicit research question used for teaching logic
#'
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#'
#' @return A single character string.
#' @keywords internal
#' @noRd
getStoredResearchQuestion = function(teachingSummary = NULL, model = NULL) {

  directResearchQuestion = trimws(as.character(attr(model, "wmfm_research_question", exact = TRUE) %||% ""))

  if (nzchar(directResearchQuestion)) {
    return(directResearchQuestion)
  }

  summaryResearchQuestion = trimws(as.character(attr(teachingSummary, "rawResearchQuestion", exact = TRUE) %||% ""))

  if (nzchar(summaryResearchQuestion)) {
    return(summaryResearchQuestion)
  }

  ""
}

#' Detect research-question framing in a sentence
#'
#' @param claimText Character scalar.
#' @param teachingSummary Optional teaching summary object.
#' @param model Optional fitted model object.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
detectResearchQuestion = function(claimText, teachingSummary = NULL, model = NULL) {

  researchQuestion = getStoredResearchQuestion(teachingSummary = teachingSummary, model = model)

  nzchar(researchQuestion) && sentenceMatchesResearchQuestion(claimText, researchQuestion)
}

#' Detect typical-case framing in a sentence
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
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
#' @noRd
detectEffect = function(claimText, audit, model = NULL) {
  sentenceMentionsModelledChange(claimText = claimText, audit = audit, model = model)
}

#' Detect uncertainty wording in a sentence
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
detectAnswer = function(
    claimText,
    sentenceIndex = NA_integer_,
    totalClaims = NA_integer_,
    model = NULL,
    teachingSummary = NULL,
    effectTag = FALSE,
    uncertaintyTag = FALSE,
    comparisonTag = FALSE,
    typicalCaseTag = FALSE
) {

  sentenceLooksLikeResearchAnswer(
    claimText = claimText,
    sentenceIndex = sentenceIndex,
    totalClaims = totalClaims,
    model = model,
    teachingSummary = teachingSummary,
    effectTag = effectTag,
    uncertaintyTag = uncertaintyTag,
    comparisonTag = comparisonTag,
    typicalCaseTag = typicalCaseTag
  )
}

#' Detect response-scale wording in a sentence
#'
#' @param claimText Character scalar.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
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
#' @noRd
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
#' @noRd
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
#' @noRd
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
      c(normaliseExplanationNumericString(row$anchor[[1]]))
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
#' @noRd
sentenceMatchesReferenceLevel = function(claimText, audit) {

  text = tolower(claimText %||% "")

  if (!is.data.frame(audit$referenceLevels) || nrow(audit$referenceLevels) == 0) {
    return(FALSE)
  }

  comparativeCue = grepl(
    "compares|compared with|compared to|relative to|reference group|reference level|other group|comparison|same for|different in|stronger|weaker|steeper|higher|lower|more|less|than in|than for",
    text,
    perl = TRUE
  )

  rowMatches = vapply(seq_len(nrow(audit$referenceLevels)), function(i) {
    row = audit$referenceLevels[i, , drop = FALSE]
    predictorMatch = grepl(tolower(row$predictor[[1]]), text, fixed = TRUE)
    referenceMatch = grepl(tolower(row$referenceLevel[[1]]), text, fixed = TRUE)
    levelMatch = FALSE

    if ("level" %in% names(row) && !is.na(row$level[[1]]) && nzchar(row$level[[1]])) {
      levelMatch = grepl(tolower(row$level[[1]]), text, fixed = TRUE)
    }

    predictorMatch || referenceMatch || levelMatch
  }, logical(1))

  parsedAvailableLevels = character(0)

  if ("levels" %in% names(audit$referenceLevels)) {
    rawLevels = stats::na.omit(as.character(audit$referenceLevels$levels))
    parsedAvailableLevels = unlist(strsplit(rawLevels, "[,|/]", perl = TRUE), use.names = FALSE)
    parsedAvailableLevels = trimws(parsedAvailableLevels)
  }

  levelTerms = unique(stats::na.omit(c(
    as.character(audit$referenceLevels$referenceLevel %||% character(0)),
    if ("level" %in% names(audit$referenceLevels)) as.character(audit$referenceLevels$level) else character(0),
    parsedAvailableLevels
  )))
  levelTerms = tolower(levelTerms[nzchar(levelTerms)])
  matchedLevels = sum(vapply(levelTerms, function(level) {
    grepl(level, text, fixed = TRUE)
  }, logical(1)))

  (comparativeCue && any(rowMatches)) ||
    (comparativeCue && matchedLevels >= 1L) ||
    matchedLevels >= 2L ||
    grepl("reference group|reference level|other group", text, perl = TRUE)
}

#' Check whether a sentence mentions a modelled change
#'
#' @param claimText Character scalar.
#' @param audit A `wmfmExplanationAudit` object.
#' @param model Optional fitted model object.
#'
#' @return Logical scalar.
#' @keywords internal
#' @noRd
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
#' @noRd
sentenceLooksLikeResearchAnswer = function(
    claimText,
    sentenceIndex = NA_integer_,
    totalClaims = NA_integer_,
    model = NULL,
    teachingSummary = NULL,
    effectTag = FALSE,
    uncertaintyTag = FALSE,
    comparisonTag = FALSE,
    typicalCaseTag = FALSE
) {

  text = trimws(tolower(claimText %||% ""))

  if (!nzchar(text)) {
    return(FALSE)
  }

  researchQuestion = getStoredResearchQuestion(
    teachingSummary = teachingSummary,
    model = model
  )

  if (!nzchar(researchQuestion)) {
    return(FALSE)
  }

  isFinalSentence = !is.na(sentenceIndex) && !is.na(totalClaims) &&
    totalClaims >= 1L && sentenceIndex == totalClaims

  if (!isFinalSentence) {
    return(FALSE)
  }

  explicitAnswerCue = grepl(
    "^(overall\\b|in summary\\b|to answer the research question\\b|this suggests\\b|this shows\\b)",
    text,
    perl = TRUE
  )

  if (isTRUE(typicalCaseTag)) {
    return(FALSE)
  }

  if (isTRUE(effectTag) && !isTRUE(uncertaintyTag)) {
    return(TRUE)
  }

  explicitAnswerCue
}
