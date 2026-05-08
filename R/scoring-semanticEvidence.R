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

  list(
    effectDirection = effectDirection,
    effectMagnitude = extractFirstNumber(
      "(increase|raises?|higher|additional|extra|difference|multiplier|double[sd]?|multiplied)[^0-9-+]{0,40}[-+]?[0-9]+([.][0-9]+)?"
    ),
    effectScale = effectScale,
    uncertaintyMentioned = detectPattern("confidence interval|95 ?%|95 percent|uncertain|weak evidence|overlap|crosses zero|includes zero|contains zero|not clear|no clear"),
    interactionAcknowledged = detectPattern("interaction|product|differs? by|difference between[^.;]{0,80}slopes|attendance effect differs"),
    comparisonMentioned = detectPattern("female|male|boys|girls|versus|compared|between|groups|similarly|difference"),
    noClearDifferenceMentioned = detectPattern("no clear|not clear|weak evidence|perform similarly|similar|overlap|includes zero|contains zero|crosses zero"),
    modelCannotAnswerQuestion = modelCannotAnswer,
    researchQuestionAnsweredDirectly = !(questionPredictorAbsent && modelCannotAnswer),
    alternativeModelInterpretationProvided = modelCannotAnswer && detectPattern("study|study effort|study hours|additional hour|odds|passing")
  )
}
