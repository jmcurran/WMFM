#' Summarise where a graded explanation lost marks
#'
#' Internal helper that converts scored WMFM grade results into compact feedback
#' tables for printing and inspection.
#'
#' @param studentScoreDf One-row scored data frame for the student answer.
#' @param modelAnswerScoreDf Optional one-row scored data frame for a reference
#'   answer.
#' @param method Character scalar. One of `"deterministic"` or `"llm"`.
#'
#' @return A named list containing feedback tables.
#'
#' @keywords internal
#' @noRd
summariseWmfmGradeLosses = function(
    studentScoreDf,
    modelAnswerScoreDf = NULL,
    method = c("deterministic", "llm")
) {

  method = match.arg(method)

  if (!is.data.frame(studentScoreDf) || nrow(studentScoreDf) != 1) {
    stop("`studentScoreDf` must be a one-row data frame.", call. = FALSE)
  }

  if (!is.null(modelAnswerScoreDf)) {
    if (!is.data.frame(modelAnswerScoreDf) || nrow(modelAnswerScoreDf) != 1) {
      stop("`modelAnswerScoreDf` must be NULL or a one-row data frame.", call. = FALSE)
    }
  }

  metricOrder = c(
    "overallScore",
    "factualScore",
    "inferenceScore",
    "completenessScore",
    "clarityScore",
    "calibrationScore",
    "effectDirectionCorrect",
    "effectScaleAppropriate",
    "referenceGroupHandledCorrectly",
    "interactionCoverageAdequate",
    "interactionSubstantiveCorrect",
    "uncertaintyHandlingAppropriate",
    "inferentialRegisterAppropriate",
    "mainEffectCoverageAdequate",
    "referenceGroupCoverageAdequate",
    "clarityAdequate",
    "numericExpressionAdequate",
    "comparisonStructureClear"
  )

  maxScoreMap = c(
    overallScore = 100,
    factualScore = 2,
    inferenceScore = 2,
    completenessScore = 2,
    clarityScore = 2,
    calibrationScore = 2,
    effectDirectionCorrect = 2,
    effectScaleAppropriate = 2,
    referenceGroupHandledCorrectly = 2,
    interactionCoverageAdequate = 2,
    interactionSubstantiveCorrect = 2,
    uncertaintyHandlingAppropriate = 2,
    inferentialRegisterAppropriate = 2,
    mainEffectCoverageAdequate = 2,
    referenceGroupCoverageAdequate = 2,
    clarityAdequate = 2,
    numericExpressionAdequate = 2,
    comparisonStructureClear = 2
  )

  missingMetrics = c(
    "interactionCoverageAdequate",
    "uncertaintyHandlingAppropriate",
    "mainEffectCoverageAdequate",
    "referenceGroupCoverageAdequate",
    "numericExpressionAdequate"
  )

  labelMap = c(
    overallScore = "Overall score",
    factualScore = "Factual score",
    inferenceScore = "Inference score",
    completenessScore = "Completeness score",
    clarityScore = "Clarity score",
    calibrationScore = "Calibration score",
    effectDirectionCorrect = "Effect direction correct",
    effectScaleAppropriate = "Effect scale appropriate",
    referenceGroupHandledCorrectly = "Reference group handled correctly",
    interactionCoverageAdequate = "Interaction coverage adequate",
    interactionSubstantiveCorrect = "Interaction substance correct",
    uncertaintyHandlingAppropriate = "Uncertainty handling appropriate",
    inferentialRegisterAppropriate = "Inferential register appropriate",
    mainEffectCoverageAdequate = "Main-effect coverage adequate",
    referenceGroupCoverageAdequate = "Reference-group coverage adequate",
    clarityAdequate = "Clarity adequate",
    numericExpressionAdequate = "Numeric expression adequate",
    comparisonStructureClear = "Comparison structure clear"
  )

  strengthMap = c(
    factualScore = "The explanation stayed close to the fitted model facts.",
    inferenceScore = "The explanation used an appropriate inferential tone.",
    completenessScore = "The explanation covered the main parts of the model story.",
    clarityScore = "The explanation was generally clear and readable.",
    calibrationScore = "The explanation kept its claims reasonably well calibrated.",
    effectDirectionCorrect = "The direction of the fitted effect was described correctly.",
    effectScaleAppropriate = "The effect was described on an appropriate scale.",
    referenceGroupHandledCorrectly = "Baseline or reference-group information was handled appropriately.",
    interactionCoverageAdequate = "Interaction structure was acknowledged where needed.",
    interactionSubstantiveCorrect = "Any interaction interpretation was substantively appropriate.",
    uncertaintyHandlingAppropriate = "Uncertainty was handled appropriately for this model.",
    inferentialRegisterAppropriate = "The wording matched the evidence level well.",
    mainEffectCoverageAdequate = "The main effect was described adequately.",
    referenceGroupCoverageAdequate = "Reference-group context was included where needed.",
    clarityAdequate = "The explanation wording was acceptably clear.",
    numericExpressionAdequate = "The explanation handled numeric information adequately.",
    comparisonStructureClear = "Comparisons were structured clearly."
  )

  parseFieldReasons = function(x) {
    if (!is.character(x) || length(x) != 1 || is.na(x) || !nzchar(trimws(x))) {
      return(list())
    }

    parsed = tryCatch(
      jsonlite::fromJSON(x, simplifyVector = TRUE),
      error = function(e) {
        NULL
      }
    )

    if (is.null(parsed)) {
      return(list())
    }

    as.list(parsed)
  }

  studentFieldReasons = parseFieldReasons(studentScoreDf$llmFieldReasons[1])
  referenceFieldReasons = list()

  if (!is.null(modelAnswerScoreDf) && "llmFieldReasons" %in% names(modelAnswerScoreDf)) {
    referenceFieldReasons = parseFieldReasons(modelAnswerScoreDf$llmFieldReasons[1])
  }

  explanationText = ""
  if ("explanationText" %in% names(studentScoreDf)) {
    explanationText = as.character(studentScoreDf$explanationText[1])
    explanationText[is.na(explanationText)] = ""
  }

  hasNumericLiteral = grepl("\\b\\d+(\\.\\d+)?\\b", explanationText, perl = TRUE)
  hasRangeIndicator = grepl("\\bbetween\\b|\\bas low as\\b|\\bas high as\\b|\\bfrom\\b.+\\bto\\b|\\b\\d+(\\.\\d+)?\\s*[-\\u2013\\u2014]\\s*\\d+(\\.\\d+)?\\b", explanationText, ignore.case = TRUE, perl = TRUE)
  mentionsOutcomeScale = grepl("0\\s*[-\\u2013\\u2014]?\\s*100|out of 100|marks?\\b|points?\\b|percent|percentage|probabilit|odds", explanationText, ignore.case = TRUE, perl = TRUE)
  effectScaleClaim = if ("effectScaleClaim" %in% names(studentScoreDf)) as.character(studentScoreDf$effectScaleClaim[1]) else NA_character_
  effectScaleClaim[is.na(effectScaleClaim)] = ""
  numericExpressionScore = if ("numericExpressionAdequate" %in% names(studentScoreDf)) suppressWarnings(as.numeric(studentScoreDf$numericExpressionAdequate[1])) else NA_real_

  buildReason = function(metric, marksLost, studentValue, maxValue) {
    if (identical(method, "llm") && length(studentFieldReasons[[metric]]) == 1) {
      return(as.character(studentFieldReasons[[metric]]))
    }

    if (identical(metric, "numericExpressionAdequate")) {
      if (!isTRUE(hasNumericLiteral)) {
        return("No clear numeric effect size or magnitude was stated.")
      }

      if (isTRUE(hasNumericLiteral) && !isTRUE(mentionsOutcomeScale) && !nzchar(effectScaleClaim)) {
        return("Numeric effect information was given, but it was not clearly interpreted relative to the outcome scale.")
      }

      if (isTRUE(hasNumericLiteral) && effectScaleClaim %in% c("not_stated", "mixed_or_unclear")) {
        return("Numeric effect information was given, but the effect scale remained unclear.")
      }

      if (isTRUE(hasNumericLiteral) && !isTRUE(hasRangeIndicator) && isTRUE(numericExpressionScore < maxValue)) {
        return("Some numeric information was given, but the quantitative interpretation was only partly developed.")
      }

      return("Numeric content was present but could be expressed more clearly or more fully.")
    }

    if (identical(metric, "effectScaleAppropriate")) {
      if (isTRUE(hasNumericLiteral) && effectScaleClaim %in% c("not_stated", "mixed_or_unclear", "")) {
        return("Numeric effects were stated, but the explanation did not make the model's effect scale clear.")
      }

      if (isTRUE(hasNumericLiteral) && !isTRUE(mentionsOutcomeScale)) {
        return("The explanation stated effect sizes, but did not anchor them clearly to the outcome scale.")
      }

      if (marksLost >= maxValue) {
        return("The explanation used an inappropriate effect scale or omitted it entirely.")
      }

      return("The explanation could use a more appropriate effect scale.")
    }

    partialReasonMap = c(
      overallScore = "The explanation lost marks across multiple rubric areas.",
      factualScore = "Some factual details were weak, incomplete, or not fully correct.",
      inferenceScore = "Inferential wording or treatment of uncertainty could be improved.",
      completenessScore = "Some relevant parts of the fitted model story were only partly covered.",
      clarityScore = "The explanation could be clearer or more clearly structured.",
      calibrationScore = "Some wording may overstate or understate what the model supports.",
      effectDirectionCorrect = "The effect direction was not stated as clearly or as accurately as it could be.",
      referenceGroupHandledCorrectly = "Reference-group handling was not completely clean.",
      interactionCoverageAdequate = "Interaction evidence was only partly acknowledged.",
      interactionSubstantiveCorrect = "Interaction interpretation was weak or only partly correct.",
      uncertaintyHandlingAppropriate = "Uncertainty was present but not handled especially well.",
      inferentialRegisterAppropriate = "The register was somewhat mismatched to the evidence.",
      mainEffectCoverageAdequate = "The main effect description was partial or underdeveloped.",
      referenceGroupCoverageAdequate = "Reference-group context was only partly covered.",
      clarityAdequate = "The wording or structure could be clearer.",
      comparisonStructureClear = "Comparison wording could be clearer or more direct."
    )

    fullReasonMap = c(
      overallScore = "The explanation lost marks across multiple rubric areas.",
      factualScore = "Key factual aspects of the fitted model were missing or incorrect.",
      inferenceScore = "The explanation did not handle inferential wording or uncertainty appropriately.",
      completenessScore = "Important parts of the model story were omitted.",
      clarityScore = "The explanation was not clear enough to communicate the fitted model well.",
      calibrationScore = "The explanation was not well calibrated to what the model can support.",
      effectDirectionCorrect = "The explanation did not state the correct effect direction.",
      referenceGroupHandledCorrectly = "The baseline or reference group was handled incorrectly.",
      interactionCoverageAdequate = "Interaction structure that needed discussion was omitted.",
      interactionSubstantiveCorrect = "The interaction interpretation was incorrect or not substantively appropriate.",
      uncertaintyHandlingAppropriate = "No appropriate uncertainty language was given.",
      inferentialRegisterAppropriate = "The register was not matched to the available evidence.",
      mainEffectCoverageAdequate = "The main effect was not described adequately.",
      referenceGroupCoverageAdequate = "Reference-group context was omitted where it was needed.",
      clarityAdequate = "The explanation wording or structure was not adequately clear.",
      comparisonStructureClear = "Comparison wording or contrast structure was not clear enough."
    )

    if (isTRUE(marksLost >= maxValue)) {
      return(unname(fullReasonMap[metric]))
    }

    unname(partialReasonMap[metric])
  }

  buildMissingDetail = function(metric, marksLost, studentValue, maxValue) {
    if (identical(metric, "numericExpressionAdequate")) {
      if (!isTRUE(hasNumericLiteral)) {
        return("No numeric effect size or quantitative interpretation was given.")
      }

      if (!isTRUE(mentionsOutcomeScale)) {
        return("Numeric effect sizes were given, but they were not interpreted relative to the outcome scale.")
      }

      if (effectScaleClaim %in% c("not_stated", "mixed_or_unclear", "")) {
        return("Numeric content was present, but the effect scale remained unclear.")
      }

      return("The quantitative interpretation was only partly developed.")
    }

    missingDetailMap = c(
      interactionCoverageAdequate = "Interaction structure that needed discussion was not mentioned.",
      uncertaintyHandlingAppropriate = "No appropriate statement about uncertainty or variability was given.",
      mainEffectCoverageAdequate = "The main fitted effect was missing or too incomplete to earn full credit.",
      referenceGroupCoverageAdequate = "Reference-group context that would help interpret the model was not developed clearly enough."
    )

    if (identical(method, "llm") && length(studentFieldReasons[[metric]]) == 1) {
      return(as.character(studentFieldReasons[[metric]]))
    }

    unname(missingDetailMap[metric])
  }

  studentVals = suppressWarnings(as.numeric(studentScoreDf[1, metricOrder, drop = FALSE]))
  maxVals = unname(maxScoreMap[metricOrder])
  marksLost = pmax(maxVals - studentVals, 0)

  referenceVals = NULL
  referenceDelta = NULL

  if (!is.null(modelAnswerScoreDf)) {
    referenceVals = suppressWarnings(as.numeric(modelAnswerScoreDf[1, metricOrder, drop = FALSE]))
    referenceDelta = pmax(referenceVals - studentVals, 0)
  }

  metricSummary = data.frame(
    metric = metricOrder,
    label = unname(labelMap[metricOrder]),
    studentValue = studentVals,
    maxValue = maxVals,
    marksLost = marksLost,
    stringsAsFactors = FALSE
  )

  metricSummary$reason = vapply(
    seq_len(nrow(metricSummary)),
    function(i) {
      buildReason(
        metric = metricSummary$metric[i],
        marksLost = metricSummary$marksLost[i],
        studentValue = metricSummary$studentValue[i],
        maxValue = metricSummary$maxValue[i]
      )
    },
    character(1)
  )

  if (!is.null(referenceVals)) {
    metricSummary$referenceValue = referenceVals
    metricSummary$referenceDelta = referenceDelta
  }

  whereMarksLost = metricSummary[
    metricSummary$marksLost > 0,
    c("metric", "label", "studentValue", "maxValue", "marksLost", "reason"),
    drop = FALSE
  ]

  if (nrow(whereMarksLost) > 0) {
    whereMarksLost = whereMarksLost[
      order(-whereMarksLost$marksLost, whereMarksLost$label),
      ,
      drop = FALSE
    ]
    rownames(whereMarksLost) = NULL
  }

  strengths = metricSummary[
    metricSummary$marksLost == 0 & metricSummary$metric != "overallScore",
    c("metric", "label", "studentValue", "maxValue"),
    drop = FALSE
  ]

  if (nrow(strengths) > 0) {
    strengths$comment = unname(strengthMap[strengths$metric])
    strengths = strengths[order(strengths$label), , drop = FALSE]
    rownames(strengths) = NULL
  }

  missingElements = metricSummary[
    metricSummary$metric %in% missingMetrics & metricSummary$marksLost > 0,
    c("metric", "label", "studentValue", "maxValue", "marksLost"),
    drop = FALSE
  ]

  if (nrow(missingElements) > 0) {
    missingElements$detail = vapply(
      seq_len(nrow(missingElements)),
      function(i) {
        buildMissingDetail(
          metric = missingElements$metric[i],
          marksLost = missingElements$marksLost[i],
          studentValue = missingElements$studentValue[i],
          maxValue = missingElements$maxValue[i]
        )
      },
      character(1)
    )
    missingElements = missingElements[
      order(-missingElements$marksLost, missingElements$label),
      ,
      drop = FALSE
    ]
    rownames(missingElements) = NULL
  }

  weaknesses = metricSummary[
    metricSummary$marksLost > 0 & !metricSummary$metric %in% c("overallScore", missingMetrics),
    c("metric", "label", "studentValue", "maxValue", "marksLost", "reason"),
    drop = FALSE
  ]

  if (nrow(weaknesses) > 0) {
    weaknesses = weaknesses[
      order(-weaknesses$marksLost, weaknesses$label),
      ,
      drop = FALSE
    ]
    rownames(weaknesses) = NULL
  }

  modelAnswerComparison = NULL

  if (!is.null(referenceVals)) {
    modelAnswerComparison = metricSummary[
      !is.na(metricSummary$referenceDelta) & metricSummary$referenceDelta > 0,
      c("metric", "label", "studentValue", "referenceValue", "referenceDelta", "reason"),
      drop = FALSE
    ]

    if (nrow(modelAnswerComparison) > 0) {
      comparisonComments = vapply(
        seq_len(nrow(modelAnswerComparison)),
        function(i) {
          metricName = modelAnswerComparison$metric[i]

          if (identical(method, "llm") && length(referenceFieldReasons[[metricName]]) == 1) {
            return(as.character(referenceFieldReasons[[metricName]]))
          }

          if (modelAnswerComparison$referenceDelta[i] >= modelAnswerComparison$referenceValue[i]) {
            return("The supplied model answer covered this area while the student answer did not.")
          }

          "The supplied model answer was stronger on this dimension."
        },
        character(1)
      )

      modelAnswerComparison$comment = comparisonComments
      modelAnswerComparison = modelAnswerComparison[
        order(-modelAnswerComparison$referenceDelta, modelAnswerComparison$label),
        ,
        drop = FALSE
      ]
      rownames(modelAnswerComparison) = NULL
    }
  }

  list(
    metricSummary = metricSummary,
    whereMarksLost = whereMarksLost,
    strengths = strengths,
    weaknesses = weaknesses,
    missingElements = missingElements,
    modelAnswerComparison = modelAnswerComparison
  )
}
