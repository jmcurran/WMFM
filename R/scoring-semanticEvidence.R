#' Semantic uncertainty wording patterns
#'
#' Returns maintained regular-expression fragments that indicate inferential
#' uncertainty in a fitted-model explanation.
#'
#' @return Character vector of regular-expression fragments.
#'
#' @keywords internal
#' @noRd
wmfmSemanticUncertaintyPatterns = function() {
  c(
    "confidence interval",
    "95 ?%",
    "95 percent",
    "uncertain",
    "weak evidence",
    "lack(s)? evidence",
    "no evidence",
    "little evidence",
    "limited evidence",
    "weak support",
    "insufficient evidence",
    "not enough evidence",
    "not enough support",
    "not enough to (show|say|conclude|confirm)",
    "overlap",
    "crosses zero",
    "includes zero",
    "contains zero",
    "includes no change",
    "compatible with no (change|difference)",
    "no change",
    "not clear",
    "no clear",
    "not statistically clear",
    "cannot confirm",
    "cannot conclude",
    "cannot say",
    "cannot distinguish",
    "do(es)? not support",
    "does not establish",
    "not established",
    "not convincing",
    "not compelling",
    "not strong enough",
    "unlikely"
  )
}

#' Semantic no-clear-difference wording patterns
#'
#' Returns maintained regular-expression fragments that indicate a comparison or
#' interaction difference is not clearly supported.
#'
#' @return Character vector of regular-expression fragments.
#'
#' @keywords internal
#' @noRd
wmfmSemanticNoClearDifferencePatterns = function() {
  c(
    "no clear",
    "not clear",
    "weak evidence",
    "lack(s)? evidence",
    "no evidence",
    "little evidence",
    "limited evidence",
    "weak support",
    "insufficient evidence",
    "not enough evidence",
    "not enough support",
    "not enough to (show|say|conclude|confirm)",
    "perform similarly",
    "similar",
    "same",
    "essentially the same",
    "overlap",
    "includes zero",
    "contains zero",
    "crosses zero",
    "includes no change",
    "compatible with no (change|difference)",
    "no change",
    "not statistically clear",
    "not clearly distinguishable",
    "cannot confirm",
    "cannot conclude",
    "cannot say",
    "cannot distinguish",
    "do(es)? not support",
    "does not establish",
    "not established",
    "does not appear to differ",
    "do not show a consistent pattern",
    "not convincing",
    "not compelling",
    "not strong enough",
    "unlikely"
  )
}

#' Extract semantic evidence from a WMFM explanation
#'
#' Builds a small structured evidence object from an explanation before rubric
#' scoring. The extractor is intentionally conservative and deterministic: it
#' records semantic evidence that is clearly present without assigning marks.
#'
#' @param explanationText Character scalar. Explanation to inspect.
#' @param modelInfo Optional named list containing fitted-model context such as
#'   `formula`, `researchQuestion`, `modelType`, `hasFactorPredictors`, and
#'   `hasInteractionTerms`.
#'
#' @return A named list of semantic evidence fields.
#'
#' @keywords internal
#' @noRd
extractWmfmSemanticEvidence = function(explanationText, modelInfo = list()) {
  text = paste(as.character(explanationText %||% ""), collapse = " ")
  text = trimws(text)
  lowerText = tolower(text)

  getInfo = function(name, default = NULL) {
    value = modelInfo[[name]]

    if (is.null(value)) {
      return(default)
    }

    value
  }

  detectPattern = function(pattern) {
    grepl(pattern, lowerText, perl = TRUE)
  }

  extractFirstNumber = function(pattern) {
    match = regexpr(pattern, lowerText, perl = TRUE)

    if (match[1] < 0) {
      return(NA_real_)
    }

    matchedText = regmatches(lowerText, match)
    numberMatch = regexpr("[-+]?[0-9]+([.][0-9]+)?", matchedText, perl = TRUE)

    if (numberMatch[1] < 0) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(regmatches(matchedText, numberMatch)))
  }

  extractCapturedNumber = function(pattern) {
    match = regexec(pattern, lowerText, perl = TRUE)
    captures = regmatches(lowerText, match)[[1]]

    if (length(captures) < 2) {
      return(NA_real_)
    }

    suppressWarnings(as.numeric(captures[2]))
  }

  formulaText = tolower(as.character(getInfo("formula", ""))[1])
  researchQuestion = tolower(as.character(getInfo("researchQuestion", ""))[1])

  predictorText = ""
  if (nzchar(formulaText) && grepl("~", formulaText, fixed = TRUE)) {
    predictorText = trimws(strsplit(formulaText, "~", fixed = TRUE)[[1]][2])
  }

  questionPredictors = character(0)
  if (grepl("test[- ]?mark|test mark|mid[- ]?term|assignment|attendance|gender|study", researchQuestion, perl = TRUE)) {
    questionPredictors = unique(unlist(regmatches(
      researchQuestion,
      gregexpr("test[- ]?mark|test mark|mid[- ]?term|assignment|attendance|gender|study", researchQuestion, perl = TRUE)
    )))
  }

  normaliseKey = function(x) {
    gsub("[^a-z0-9]", "", tolower(x), perl = TRUE)
  }

  questionPredictorKeys = normaliseKey(questionPredictors)
  predictorKey = normaliseKey(predictorText)

  questionPredictorAbsent = length(questionPredictorKeys) > 0 &&
    nzchar(predictorKey) &&
    !any(vapply(questionPredictorKeys, grepl, logical(1), x = predictorKey, fixed = TRUE))

  modelCannotAnswer = detectPattern(
    paste(
      "does not include|doesn't include|not include",
      "cannot (directly )?(answer|tell|explain|estimate|infer)",
      "does not (directly )?(answer|provide|estimate)",
      "cannot be inferred",
      "not provide (any )?estimate",
      sep = "|"
    )
  )

  extractGroupEstimate = function(groupPattern) {
    numberPattern = "([-+]?[0-9]+([.][0-9]+)?)"
    numberBeforeGroupPattern = paste0(
      numberPattern,
      "[^0-9.;,]{0,80}\\b(?:",
      groupPattern,
      ")\\b"
    )
    groupBeforeNumberPattern = paste0(
      "\\b(?:",
      groupPattern,
      ")\\b",
      "(?:(?!confidence interval|95 ?%|95 percent|ci)[^0-9.;,]){0,80}",
      numberPattern
    )

    for (pattern in c(numberBeforeGroupPattern, groupBeforeNumberPattern)) {
      estimate = extractCapturedNumber(pattern)

      if (!is.na(estimate)) {
        return(estimate)
      }
    }

    NA_real_
  }

  femaleEstimate = extractGroupEstimate("female|girls?")
  maleEstimate = extractGroupEstimate("male|boys?")
  numericPositiveComparison = !is.na(femaleEstimate) &&
    !is.na(maleEstimate) &&
    femaleEstimate > maleEstimate
  numericNegativeComparison = !is.na(femaleEstimate) &&
    !is.na(maleEstimate) &&
    femaleEstimate < maleEstimate

  positiveEvidence = detectPattern(
    paste(
      "higher|raises?|increase[sd]?|additional|extra|positive|larger|more",
      "double[sd]?|multiplied|odds[^.;]{0,40}2|associated with[^.;]{0,80}higher",
      sep = "|"
    )
  ) || numericPositiveComparison
  negativeEvidence = detectPattern("lower|decrease[sd]?|negative|smaller|reduc") ||
    numericNegativeComparison

  effectDirection = "not_stated"
  if (positiveEvidence && negativeEvidence) {
    effectDirection = "mixed_or_both"
  } else if (positiveEvidence) {
    effectDirection = "positive"
  } else if (negativeEvidence) {
    effectDirection = "negative"
  }

  multiplicativeEvidence = detectPattern("odds|multiplier|double[sd]?|multiplied|times")
  additiveEvidence = detectPattern("point|mark|score|unit|slope|mean|average|difference") &&
    !multiplicativeEvidence
  effectScale = "not_stated"
  if (additiveEvidence) {
    effectScale = "additive"
  } else if (multiplicativeEvidence) {
    effectScale = "multiplicative"
  }

  uncertaintyMentioned = detectPattern(paste(
    wmfmSemanticUncertaintyPatterns(),
    collapse = "|"
  ))
  noClearDifferenceMentioned = detectPattern(paste(
    wmfmSemanticNoClearDifferencePatterns(),
    collapse = "|"
  ))

  list(
    effectDirection = effectDirection,
    effectMagnitude = extractFirstNumber(
      "(increase|raises?|higher|additional|extra|difference|multiplier|double[sd]?|multiplied)[^0-9-+]{0,40}[-+]?[0-9]+([.][0-9]+)?"
    ),
    effectScale = effectScale,
    uncertaintyMentioned = uncertaintyMentioned,
    uncertaintyPresent = uncertaintyMentioned,
    interactionAcknowledged = detectPattern("interaction|product|differs? by|difference between[^.;]{0,80}slopes|attendance effect differs"),
    comparisonMentioned = detectPattern("female|male|boys|girls|versus|compared|between|groups|similarly|difference"),
    noClearDifferenceMentioned = noClearDifferenceMentioned,
    noClearDifference = noClearDifferenceMentioned,
    modelCannotAnswerQuestion = modelCannotAnswer,
    researchQuestionAnsweredDirectly = !(questionPredictorAbsent && modelCannotAnswer),
    alternativeModelInterpretationProvided = modelCannotAnswer && detectPattern("study|study effort|study hours|additional hour|odds|passing")
  )
}

#' Build semantic evidence diagnostics from extracted evidence
#'
#' Converts the compact semantic evidence list into a stable diagnostics table.
#' This table is informational only and is not used to calculate marks.
#'
#' @param evidence A named list produced by `extractWmfmSemanticEvidence()`.
#'
#' @return A data frame with one row per semantic evidence field.
#'
#' @keywords internal
#' @noRd
buildWmfmSemanticEvidenceDiagnostics = function(evidence) {
  if (!is.list(evidence)) {
    evidence = list()
  }

  fieldOrder = c(
    "effectDirection",
    "effectMagnitude",
    "effectScale",
    "uncertaintyMentioned",
    "interactionAcknowledged",
    "comparisonMentioned",
    "noClearDifferenceMentioned",
    "modelCannotAnswerQuestion",
    "researchQuestionAnsweredDirectly",
    "alternativeModelInterpretationProvided"
  )

  labelMap = c(
    effectDirection = "Effect direction",
    effectMagnitude = "Effect magnitude",
    effectScale = "Effect scale",
    uncertaintyMentioned = "Uncertainty mentioned",
    interactionAcknowledged = "Interaction acknowledged",
    comparisonMentioned = "Comparison mentioned",
    noClearDifferenceMentioned = "No-clear-difference language",
    modelCannotAnswerQuestion = "Model cannot answer question",
    researchQuestionAnsweredDirectly = "Research question answered directly",
    alternativeModelInterpretationProvided = "Alternative model interpretation provided"
  )

  describeValue = function(field, value) {
    if (length(value) == 0 || is.null(value)) {
      value = NA
    }

    if (is.logical(value)) {
      if (isTRUE(value)) {
        return("present")
      }

      if (identical(value, FALSE)) {
        return("absent")
      }

      return("unknown")
    }

    if (is.numeric(value)) {
      if (length(value) < 1 || is.na(value[1])) {
        return("not stated")
      }

      return(as.character(round(value[1], 4)))
    }

    valueText = as.character(value[1])
    if (is.na(valueText) || !nzchar(valueText)) {
      return("not stated")
    }

    valueText
  }

  evidencePresent = function(field, value) {
    if (length(value) == 0 || is.null(value)) {
      return(FALSE)
    }

    if (is.logical(value)) {
      return(isTRUE(value))
    }

    if (is.numeric(value)) {
      return(!is.na(value[1]))
    }

    valueText = as.character(value[1])
    !is.na(valueText) && nzchar(valueText) && !valueText %in% c("not_stated", "unknown")
  }

  detailMap = c(
    effectDirection = "Direction inferred from directional wording or group estimates.",
    effectMagnitude = "First clearly stated fitted magnitude found near effect wording.",
    effectScale = "Scale inferred from additive or multiplicative model language.",
    uncertaintyMentioned = "Uncertainty language such as confidence intervals, overlap, or weak evidence.",
    interactionAcknowledged = "Interaction or slope-difference structure mentioned.",
    comparisonMentioned = "Group comparison or contrast language mentioned.",
    noClearDifferenceMentioned = "Language indicating no clear or well-supported difference.",
    modelCannotAnswerQuestion = "Explanation says the fitted model cannot directly answer the research question.",
    researchQuestionAnsweredDirectly = "Whether the fitted model is treated as directly answering the research question.",
    alternativeModelInterpretationProvided = "Explanation gives the fitted predictor interpretation when the research question predictor is absent."
  )

  data.frame(
    field = fieldOrder,
    label = unname(labelMap[fieldOrder]),
    value = vapply(fieldOrder, function(field) {
      describeValue(field, evidence[[field]])
    }, character(1)),
    evidencePresent = vapply(fieldOrder, function(field) {
      evidencePresent(field, evidence[[field]])
    }, logical(1)),
    detail = unname(detailMap[fieldOrder]),
    stringsAsFactors = FALSE
  )
}

#' Extract semantic evidence diagnostics for a grade object
#'
#' Builds semantic evidence diagnostics using the explanation and model context
#' stored in a `wmfmGrade` object. This helper does not score or rescore the
#' explanation.
#'
#' @param gradeObj A `wmfmGrade` object.
#'
#' @return A semantic evidence diagnostics data frame.
#'
#' @keywords internal
#' @noRd
buildWmfmGradeSemanticEvidenceDiagnostics = function(gradeObj) {
  if (!inherits(gradeObj, "wmfmGrade")) {
    stop("`gradeObj` must inherit from `wmfmGrade`.", call. = FALSE)
  }

  modelObj = gradeObj$model
  modelInfo = list(
    formula = paste(deparse(modelObj$formula), collapse = " "),
    researchQuestion = modelObj$researchQuestion %||% "",
    modelType = modelObj$modelType %||% "",
    hasFactorPredictors = length(modelObj$modelProfile$factorTerms %||% character(0)) > 0,
    hasInteractionTerms = length(modelObj$interactionTerms %||% character(0)) > 0
  )

  evidence = extractWmfmSemanticEvidence(
    explanationText = gradeObj$input$explanation,
    modelInfo = modelInfo
  )

  buildWmfmSemanticEvidenceDiagnostics(evidence)
}



#' Build semantic evidence for run-record scoring
#'
#' Extracts semantic evidence row-by-row from run records and returns a compact
#' data frame that can be joined to deterministic scoring diagnostics. This
#' helper is intentionally conservative: it can strengthen missing or unclear
#' extracted claims, but it does not assign marks directly.
#'
#' @param runsDf A run-record data frame.
#'
#' @return A data frame with one row per run record.
#'
#' @keywords internal
#' @noRd
buildWmfmRunRecordSemanticEvidence = function(runsDf) {
  if (!is.data.frame(runsDf)) {
    stop("`runsDf` must be a data.frame.", call. = FALSE)
  }

  getColumnValue = function(row, name, default = "") {
    if (!(name %in% names(row))) {
      return(default)
    }

    value = row[[name]][1]

    if (length(value) == 0 || is.na(value)) {
      return(default)
    }

    value
  }

  normaliseDirection = function(value) {
    value = as.character(value %||% "not_stated")[1]

    if (identical(value, "positive")) {
      return("increase")
    }

    if (identical(value, "negative")) {
      return("decrease")
    }

    if (identical(value, "mixed_or_both")) {
      return("mixed_or_both")
    }

    "not_stated"
  }

  normaliseScale = function(value) {
    value = as.character(value %||% "not_stated")[1]

    if (value %in% c("additive", "multiplicative")) {
      return(value)
    }

    "not_stated"
  }

  rows = lapply(seq_len(nrow(runsDf)), function(i) {
    row = runsDf[i, , drop = FALSE]
    evidence = extractWmfmSemanticEvidence(
      explanationText = getColumnValue(row, "explanationText", ""),
      modelInfo = list(
        formula = getColumnValue(row, "formula", ""),
        researchQuestion = getColumnValue(row, "researchQuestion", ""),
        modelType = getColumnValue(row, "modelType", ""),
        hasFactorPredictors = isTRUE(as.logical(getColumnValue(row, "hasFactorPredictors", FALSE))),
        hasInteractionTerms = isTRUE(as.logical(getColumnValue(row, "hasInteractionTerms", FALSE)))
      )
    )

    data.frame(
      semanticEffectDirection = evidence$effectDirection,
      semanticEffectDirectionClaim = normaliseDirection(evidence$effectDirection),
      semanticEffectScale = evidence$effectScale,
      semanticEffectScaleClaim = normaliseScale(evidence$effectScale),
      semanticComparisonMentioned = isTRUE(evidence$comparisonMentioned),
      semanticUncertaintyMentioned = isTRUE(evidence$uncertaintyMentioned),
      semanticNoClearDifferenceMentioned = isTRUE(evidence$noClearDifferenceMentioned),
      semanticInteractionAcknowledged = isTRUE(evidence$interactionAcknowledged),
      semanticModelCannotAnswerQuestion = isTRUE(evidence$modelCannotAnswerQuestion),
      semanticResearchQuestionAnsweredDirectly = isTRUE(evidence$researchQuestionAnsweredDirectly),
      semanticAlternativeModelInterpretationProvided = isTRUE(evidence$alternativeModelInterpretationProvided),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}
