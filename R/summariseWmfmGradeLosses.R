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

  reasonMap = c(
    overallScore = "The explanation lost marks across one or more rubric dimensions.",
    factualScore = "Some factual aspects of the fitted model were missing, weak, or inaccurate.",
    inferenceScore = "The explanation's inferential language or uncertainty handling was not fully appropriate.",
    completenessScore = "Important parts of the model story were omitted or only partly covered.",
    clarityScore = "The explanation could be clearer, better structured, or better expressed.",
    calibrationScore = "The explanation may overclaim, underclaim, or miscalibrate evidential strength.",
    effectDirectionCorrect = "The explanation did not clearly state the correct effect direction.",
    effectScaleAppropriate = "The explanation did not use an appropriate effect scale.",
    referenceGroupHandledCorrectly = "The reference or baseline group was not handled cleanly.",
    interactionCoverageAdequate = "Interaction structure was not fully acknowledged.",
    interactionSubstantiveCorrect = "The substantive interaction interpretation was weak or incorrect.",
    uncertaintyHandlingAppropriate = "Uncertainty was missing, unclear, or not handled appropriately.",
    inferentialRegisterAppropriate = "The register was too strong, too weak, or otherwise not well matched to the evidence.",
    mainEffectCoverageAdequate = "The main effect was not described adequately.",
    referenceGroupCoverageAdequate = "Reference-group context was omitted where it was needed.",
    clarityAdequate = "The wording or structure was not as clear as it could be.",
    numericExpressionAdequate = "Numeric content was missing, awkward, or not expressed well enough.",
    comparisonStructureClear = "Comparison wording or contrast structure was not fully clear."
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
        reason = unname(reasonMap[[metric]]),
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
    whereMarksLost = whereMarksLost[order(whereMarksLost$marksLost, decreasing = TRUE), , drop = FALSE]
    rownames(whereMarksLost) = NULL
  }

  strengths = metricSummary[
    metricSummary$marksLost == 0,
    c("metric", "label", "studentValue", "maxValue"),
    drop = FALSE
  ]

  if (nrow(strengths) > 0) {
    strengths = strengths[order(strengths$maxValue, strengths$label, decreasing = TRUE), , drop = FALSE]
    rownames(strengths) = NULL
  }

  modelAnswerComparison = NULL

  if (!is.null(referenceVals)) {
    modelAnswerComparison = metricSummary[
      !is.na(metricSummary$referenceDelta) & metricSummary$referenceDelta > 0,
      c("metric", "label", "studentValue", "referenceValue", "referenceDelta", "reason"),
      drop = FALSE
    ]

    if (nrow(modelAnswerComparison) > 0) {
      modelAnswerComparison = modelAnswerComparison[
        order(modelAnswerComparison$referenceDelta, decreasing = TRUE),
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
    modelAnswerComparison = modelAnswerComparison
  )
}
