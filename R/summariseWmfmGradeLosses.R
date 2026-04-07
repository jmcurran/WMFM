#' Summarise where a graded explanation lost marks
#'
#' Internal helper that converts scored WMFM grade results into compact feedback
#' tables for printing and inspection.
#'
#' @param studentScoreDf One-row scored data frame for the student answer.
#' @param modelAnswerScoreDf Optional one-row scored data frame for a reference
#'   answer.
#'
#' @return A named list containing feedback tables.
#'
#' @keywords internal
#' @noRd
summariseWmfmGradeLosses = function(
    studentScoreDf,
    modelAnswerScoreDf = NULL
) {

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

  partialReasonMap = c(
    overallScore = "The explanation lost marks across multiple rubric areas.",
    factualScore = "Some factual details were weak, incomplete, or not fully correct.",
    inferenceScore = "Inferential wording or treatment of uncertainty could be improved.",
    completenessScore = "Some relevant parts of the fitted model story were only partly covered.",
    clarityScore = "The explanation could be clearer or more clearly structured.",
    calibrationScore = "Some wording may overstate or understate what the model supports.",
    effectDirectionCorrect = "The effect direction was not stated as clearly or as accurately as it could be.",
    effectScaleAppropriate = "The explanation could use a more appropriate effect scale.",
    referenceGroupHandledCorrectly = "Reference-group handling was not completely clean.",
    interactionCoverageAdequate = "Interaction evidence was only partly acknowledged.",
    interactionSubstantiveCorrect = "Interaction interpretation was weak or only partly correct.",
    uncertaintyHandlingAppropriate = "Uncertainty was present but not handled especially well.",
    inferentialRegisterAppropriate = "The register was somewhat mismatched to the evidence.",
    mainEffectCoverageAdequate = "The main effect description was partial or underdeveloped.",
    referenceGroupCoverageAdequate = "Reference-group context was only partly covered.",
    clarityAdequate = "The wording or structure could be clearer.",
    numericExpressionAdequate = "Numeric content was present but could be expressed more clearly or more fully.",
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
    effectScaleAppropriate = "The explanation used an inappropriate effect scale or omitted it entirely.",
    referenceGroupHandledCorrectly = "The baseline or reference group was handled incorrectly.",
    interactionCoverageAdequate = "Interaction structure that needed discussion was omitted.",
    interactionSubstantiveCorrect = "The interaction interpretation was incorrect or not substantively appropriate.",
    uncertaintyHandlingAppropriate = "No appropriate uncertainty language was given.",
    inferentialRegisterAppropriate = "The register was not matched to the available evidence.",
    mainEffectCoverageAdequate = "The main effect was not described adequately.",
    referenceGroupCoverageAdequate = "Reference-group context was omitted where it was needed.",
    clarityAdequate = "The explanation wording or structure was not adequately clear.",
    numericExpressionAdequate = "No usable numeric effect information was given.",
    comparisonStructureClear = "Comparison wording or contrast structure was not clear enough."
  )

  missingDetailMap = c(
    interactionCoverageAdequate = "Interaction structure that needed discussion was not mentioned.",
    uncertaintyHandlingAppropriate = "No appropriate statement about uncertainty or variability was given.",
    mainEffectCoverageAdequate = "The main fitted effect was missing or too incomplete to earn full credit.",
    referenceGroupCoverageAdequate = "The explanation did not give the needed reference-group context.",
    numericExpressionAdequate = "No usable numeric effect size or magnitude was stated."
  )

  makeLabel = function(metric) {
    info = describeWmfmField(metric, format = "list", includeExamples = FALSE, includeAliases = FALSE)

    if (is.list(info) && !is.null(info$title) && nzchar(info$title)) {
      return(info$title)
    }

    metric
  }

  studentVals = studentScoreDf[1, metricOrder, drop = FALSE]

  if (!is.null(modelAnswerScoreDf)) {
    referenceVals = modelAnswerScoreDf[1, metricOrder, drop = FALSE]
  } else {
    referenceVals = NULL
  }

  rows = lapply(
    metricOrder,
    function(metric) {
      studentValue = suppressWarnings(as.numeric(studentVals[[metric]]))
      maxValue = unname(maxScoreMap[[metric]])
      marksLost = max(0, maxValue - studentValue)
      referenceValue = NA_real_
      referenceDelta = NA_real_
      reason = if (marksLost >= maxValue) {
        unname(fullReasonMap[[metric]])
      } else if (marksLost > 0) {
        unname(partialReasonMap[[metric]])
      } else {
        NA_character_
      }

      if (!is.null(referenceVals)) {
        referenceValue = suppressWarnings(as.numeric(referenceVals[[metric]]))
        referenceDelta = referenceValue - studentValue
      }

      data.frame(
        metric = metric,
        label = makeLabel(metric),
        studentValue = studentValue,
        maxValue = maxValue,
        marksLost = marksLost,
        referenceValue = referenceValue,
        referenceDelta = referenceDelta,
        reason = reason,
        stringsAsFactors = FALSE
      )
    }
  )

  metricSummary = do.call(rbind, rows)
  rownames(metricSummary) = NULL

  whereMarksLost = metricSummary[
    metricSummary$marksLost > 0,
    c("metric", "label", "studentValue", "maxValue", "marksLost", "reason"),
    drop = FALSE
  ]

  if (nrow(whereMarksLost) > 0) {
    whereMarksLost = whereMarksLost[
      order(whereMarksLost$marksLost, whereMarksLost$label, decreasing = c(TRUE, FALSE)),
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
    missingElements$detail = unname(missingDetailMap[missingElements$metric])
    missingElements = missingElements[
      order(missingElements$marksLost, missingElements$label, decreasing = c(TRUE, FALSE)),
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
      order(weaknesses$marksLost, weaknesses$label, decreasing = c(TRUE, FALSE)),
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
      comparisonComment = ifelse(
        modelAnswerComparison$referenceDelta >= modelAnswerComparison$referenceValue,
        "The supplied model answer covered this area while the student answer did not.",
        "The supplied model answer was stronger on this dimension."
      )
      modelAnswerComparison$comment = comparisonComment
      modelAnswerComparison = modelAnswerComparison[
        order(modelAnswerComparison$referenceDelta, modelAnswerComparison$label, decreasing = c(TRUE, FALSE)),
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
